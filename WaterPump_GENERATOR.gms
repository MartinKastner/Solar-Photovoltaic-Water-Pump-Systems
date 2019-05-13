Set      h        hours per year                         /h1*h8760/
         d        days per year                          /d1*d365/
         pump     pump models                            /gp1*gp24/
         head     pump different head levels             /20,40,60,80,100/
         pchar    pump characteristics of pumps          /head,flow,InvestCostPump,InvestCostGen,fueldem,EACtotal,EACpump,EACgenerator/
         Eco      economic characteristics               /TankCostPerCubic,Installation,InvestCostPerCubic,CostofCapital,ExpectLifetime,EAC/
         Scen     scenarios for different water demands  /W1*W6/
         t        time in years                          /t0*t10/
         param    parameters for financial analysis      /ancosts,sumcosts,totalcosts/;

alias(t,s);

Parameters WaterCases(Scen) /
W1   5
W2  10
W3  20
W4  30
W5  40
W6  50
/;

Parameters       Summary(*,*)
                 CostParameters(*,*)
                 InvestmentCosts(*,*)
                 AnnualCosts(*,*)
                 TechnicalReport(*,*)
                 HourlyFlow(*,*,*)
                 DailyFlow(*,*,*);

Parameters       hind(h,d)
                 PumpData(pump,pchar)
                 workinghours(h)
                 TankCost(Eco)
                 output(scen,t,param);

$Onecho > WaterPump_GENERATOR.txt
Par=hind         rng=hoursperday!A1      rdim=2
Par=pumpData     rng=GASpumps!A1         rdim=1  cdim=1
Par=workinghours rng=GENon!A1            rdim=1
Par=tankcost     rng=costs!A12           rdim=1
$Offecho

$CALL GDXXRW 201904_InputData.xlsx output=.\WaterPump_GENERATOR.gdx squeeze=N trace=3 @WaterPump_GENERATOR.txt
$GDXIN .\WaterPump_GENERATOR.gdx
$Load hind pumpdata workinghours tankcost
$GDXIN

Display hind,pumpdata,workinghours,tankcost;

Scalars  WaterRef        Reference value for water demand                /1/
         WaterWell       Depth of the well                               /20/
         FuelPrice       cost per liter gasoline                         /0.5/
         INVCost         investment costs of the pump system
         OPcost          maintenance costs of the pump system
         MaintenancePump Annual Maintenance Cost in percent              /0.05/
         MaintenanceGen  Annual Maintenance Cost in percent              /0.1/
         i               interest rate                                   /0.20/;

Free Variables TotCost;
Variable x_tank_out(h);

Positive variable x_pumpflow(h,pump),x_tank_level(h),x_tank_capacity;

Binary variable x_b_pump_on(h,pump),x_b_invest_pump(pump);

Equations
TotalCost
DailyWaterDemand
TankLevel
TankCapacity
WaterFlow
PumpExists
LimitPumpDepth
LimitPumpInvest;


TotalCost..                      TotCost =E=
                                 sum((h,pump),x_b_pump_on(h,pump) * PumpData(pump,'fueldem') * FuelPrice)
                                 + sum((pump),PumpData(pump,'EACtotal')* x_b_invest_pump(pump))
                                 + x_tank_capacity * TankCost('EAC');

DailyWaterDemand(d)..            WaterRef                        =E=     sum(h$hind(h,d),x_tank_out(h));

TankLevel(h)..                   x_tank_level(h)                 =E=     x_tank_level(h-1) - x_tank_out(h) + sum(pump,x_pumpflow(h,pump));

TankCapacity(h)..                x_tank_level(h)                 =L=     x_tank_capacity;


Waterflow(h,pump)..              x_pumpflow(h,pump)              =L=     PumpData(pump,'flow') * x_b_pump_on(h,pump) * workinghours(h);


PumpExists(h,pump)..             x_b_pump_on(h,pump)             =L=     x_b_invest_pump(pump);

LimitPumpDepth..                 WaterWell                       =E=     sum((pump),PumpData(pump,'head') * x_b_invest_pump(pump));

LimitPumpInvest..                sum((pump),x_b_invest_pump(pump))  =L=     1;


Model waterpump /all/;

option optcr=0.01;
option reslim=3000;

Loop(Scen,
WaterRef =  WaterCases(Scen)
Solve Waterpump using MIP minimizing TotCost;

Summary(Scen,'DailyWater')               =WaterRef;
Summary(Scen,'WellHead')                 =WaterWell;

CostParameters(Scen,'FuelPrice')         =FuelPrice;
CostParameters(Scen,'TankPrice')         =TankCost('TankCostPerCubic');


InvestmentCosts(Scen,'TotInvest')        = sum((pump),PumpData(pump,'InvestCostPump')* x_b_invest_pump.l(pump))
                                         + sum((pump),Pumpdata(pump,'InvestCostGen') * x_b_invest_pump.l(pump))
                                         + x_tank_capacity.l * TankCost('InvestCostPerCubic');
InvestmentCosts(Scen,'InvestPump')       = sum((pump),PumpData(pump,'InvestCostPump')* x_b_invest_pump.l(pump));
InvestmentCosts(Scen,'InvestGen')        = sum((pump),Pumpdata(pump,'InvestCostGen') * x_b_invest_pump.l(pump));
InvestmentCosts(Scen,'InvestTank')       = x_tank_capacity.l * TankCost('InvestCostPerCubic');


AnnualCosts(Scen,'TotCost')              = TotCost.l;
AnnualCosts(Scen,'CostPerCubic')         = TotCost.l/sum((h,pump),x_pumpflow.l(h,pump));
AnnualCosts(Scen,'PumpEAC')              = sum((pump),(x_b_invest_pump.l(pump) * PumpData(pump,'EACpump')));
AnnualCosts(Scen,'GenEAC')               = sum((pump),(x_b_invest_pump.l(pump) * PumpData(pump,'EACgenerator')));
AnnualCosts(Scen,'FuelCosts')            = sum((h,pump),x_b_pump_on.l(h,pump) * PumpData(pump,'fueldem') * FuelPrice);
AnnualCosts(Scen,'TankEAC')              = x_tank_capacity.l * TankCost('EAC');


TechnicalReport(Scen,'TankCap')          = x_tank_capacity.l;
TechnicalReport(Scen,'PumpHours')        = sum((h,pump),x_b_pump_on.l(h,pump));
TechnicalReport(Scen,'PumpHoursPday')    = sum((h,pump),x_b_pump_on.l(h,pump))/card(d);


HourlyFlow(Scen,h,'HwaterFlow')          = sum(pump,x_pumpflow.l(h,pump));

DailyFlow(Scen,d,'DwaterFlow')           = sum((h,pump)$hind(h,d),x_pumpflow.l(h,pump));



INVCost=         sum((pump),PumpData(pump,'InvestCostPump')* x_b_invest_pump.l(pump))
                 + sum((pump),PumpData(pump,'InvestCostGen') * x_b_invest_pump.l(pump))
                 + x_tank_capacity.l * TankCost('InvestCostPerCubic');

OPcost=          sum((pump),PumpData(pump,'InvestCostPump')* x_b_invest_pump.l(pump)) * MaintenancePump
                 + sum((pump),PumpData(pump,'InvestCostGen') * x_b_invest_pump.l(pump)) * MaintenanceGen
                 + sum((h,pump),x_b_pump_on.l(h,pump) * PumpData(pump,'fueldem') * FuelPrice);

output(scen,t,"ancosts")$(ord(t)<11) = OPcost * ((1+i)**(-(ord(t)-1)));
output(scen,t,"sumcosts") = sum(s$(ord(s)<ord(t)),output(scen,s,"ancosts"));
output(scen,t,"totalcosts")$(ord(t)>1)=INVCost+output(scen,t,"sumcosts");
);

option InvestmentCosts:4:1:1;

Display summary,CostParameters,InvestmentCosts,AnnualCosts,TechnicalReport,HourlyFlow,DailyFlow,output;


$Onecho > WaterPump_Gasoline_Results.txt
par=summary              rng=Tabelle1!A1         rdim=1 cdim=1
par=CostParameters       rng=Tabelle1!A10        rdim=1 cdim=1
par=InvestmentCosts      rng=Tabelle1!A20        rdim=1 cdim=1
par=AnnualCosts          rng=Tabelle1!A30        rdim=1 cdim=1
par=TechnicalReport      rng=Tabelle1!A40        rdim=1 cdim=1
par=output               rng=Tabelle1!J1         rdim=2 cdim=1
$offecho

execute_unload 'Results.gdx';
execute 'gdxxrw Results.gdx output=201903_Results_GEN_Scen4_20m_TEST.xlsx squeeze=N trace=3 @WaterPump_Gasoline_Results.txt';



