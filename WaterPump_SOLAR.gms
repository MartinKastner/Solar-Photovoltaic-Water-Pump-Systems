Set      h         hours per year                        /h1*h8760/
         d         days per year                         /d1*d365/
         m         months per year                       /m1*m12/
         pump      pump models                           /pvp1*pvp12/
         head      pump different head levels            /20,40,60,80,100/
         pchar     pump characteristics of pumps         /H_max,P_min,P_max,k,d/
         Eco       economic characteristics              /InvestCostPump,PVCostPerKW,Installation,InvestCostPerKW,TankCostPerCubic,InvestCostPerCubic,CostOfCapital,ExpectLifetime,AnnualMaintCost,EAC/
         Scen      Scenarios for different water demands /W1*W6/
         t         time in years                         /t0*t10/
         finance   parameters for financial analysis     /ancosts,sumcosts,totalcosts/;

alias(t,s);

Parameter WaterCases(Scen) /
W1   5
W2  10
W3  20
W4  30
W5  40
W6  50
/;

Parameters       Summary(*,*)
                 CostParameters
                 InvestmentCosts(*,*)
                 AnnualCosts(*,*)
                 TechnicalReport(*,*)
                 report2(*,*,*)
                 DailyFlow(*,*,*);

Parameters       solar(h)
                 hind(h,d)
                 dinm(d,m)
                 PumpTech(pump,head,pchar)
                 PumpCost(Eco,pump)
                 PVcost(Eco)
                 TankCost(Eco)
                 output(scen,t,finance);

$Onecho > WaterPump_SOLAR.txt
Par=solar        rng=SolarData!B3        rdim=1
Par=hind         rng=hoursperday!A1      rdim=2
Par=dinm         rng=dayspermonth!A1     rdim=2
Par=pumptech     rng=pvpumps!C2          rdim=2 cdim=1
Par=pumpcost     rng=pvpumps!A41         rdim=1 cdim=1
Par=pvcost       rng=costs!A2            rdim=1
Par=tankcost     rng=costs!A12           rdim=1
$Offecho

$CALL GDXXRW 201904_InputData.xlsx output=.\WaterPump_SOLAR.gdx squeeze=N trace=3 @WaterPump_SOLAR.txt
$GDXIN .\WaterPump_SOLAR.gdx
$Load solar hind pumptech pumpcost dinm pvcost Tankcost
$GDXIN

Display solar,hind,dinm,pumptech,pumpcost,pvcost,Tankcost;

Parameter dailysolar(d);
dailysolar(d)=sum(h$hind(h,d),solar(h));
Display dailysolar;

Parameter monthlysolaraverage(m);
monthlysolaraverage(m)=sum(d$dinm(d,m),dailysolar(d))/sum(d,dinm(d,m));
Display monthlysolaraverage;


Scalars  WaterRef        Reference value for water demand                /1/
         WaterWell       depth of the well                               /100/
         PVeff           total efficiency of the PV system               /0.85/
         INVCost         investment costs of the pump system
         OPcost          maintenance costs of the pump system
         MaintenancePump Annual Maintenance Cost in percent              /0.10/
         i               interest rate                                   /0.20/;

Free Variables   TotCost         Total Cost of the system;
Variable         x_tank_out(h)   water withdrawal from the tank;

Binary Variable
x_b_pump_on(h,pump,head)      determine if the pump is "on"
x_b_invest_pump(pump,head)    invest or not in component;

Positive Variable
x_tank_capacity         ,  x_tank_level(h)       tank related variables
x_pumpflow(h,pump,head) ,  x_pump(h,pump,head)   pump related variables
x_PV_Capacity                                    pv   related variable ;

Equations
TotalCost
DailyWaterDemand
TankLevel
TankCapacity
Waterflow
PVPowerLimit
LimitPumpLow
LimitPumpHigh
PumpExists
LimitPumpDepth
LimitPumpInvest;

TotalCost..                      TotCost =E=
                                 x_PV_capacity * PVcost('EAC')
                                 + sum((pump,head),(x_b_invest_pump(pump,head) * PumpCost('EAC',pump)))
                                 + x_tank_capacity * TankCost('EAC');


DailyWaterDemand(d)..            WaterRef                                =E=     sum(h$hind(h,d),x_tank_out(h));

TankLevel(h)..                   x_tank_level(h)                         =E=     x_tank_level(h-1) - x_tank_out(h) + sum((pump,head),x_pumpflow(h,pump,head));

TankCapacity(h)..                x_tank_level(h)                         =L=     x_tank_capacity;

Waterflow(h,pump,head)..         x_pumpflow(h,pump,head)                 =E=     x_pump(h,pump,head) * PumpTech(pump,head,'k')   +   PumpTech(pump,head,'d') * x_b_pump_on(h,pump,head);


PVPowerLimit(h)..                sum((pump,head),x_pump(h,pump,head))    =L=     solar(h) * x_PV_Capacity * PVeff;

LimitPumpLow(h,pump,head)..      x_pump(h,pump,head)                     =G=     PumpTech(pump,head,'P_min') * x_b_pump_on(h,pump,head);

LimitPumpHigh(h,pump,head)..     x_pump(h,pump,head)                     =L=     PumpTech(pump,head,'P_max') * x_b_pump_on(h,pump,head);

PumpExists(h,pump,head)..        x_b_pump_on(h,pump,head)                =L=     x_b_invest_pump(pump,head);

LimitPumpDepth..                 WaterWell                               =E=     sum((pump,head),PumpTech(pump,head,'H_max') * x_b_invest_pump(pump,head));

LimitPumpInvest..                sum((pump,head),x_b_invest_pump(pump,head))     =L= 1;


Model Waterpump /all/

option optcr=0.001;
option reslim=2400;


Loop(Scen,
WaterRef = WaterCases(Scen)
Solve Waterpump using MIP minimizing TotCost;


summary(Scen,'Dailywater')=WaterRef;
summary(Scen,'WellHead')=WaterWell;

CostParameters(Scen,'PVPrice')           =PVcost('PVCostPerKW');
CostParameters(Scen,'TankPrice')         =TankCost('TankCostPerCubic');

InvestmentCosts(Scen,'TotInvest')        =     x_PV_capacity.l *PVcost('InvestCostPerKW')
                                            +  sum((pump,head),PumpCost('InvestCostPump',pump)* x_b_invest_pump.l(pump,head))
                                            +  x_tank_capacity.l * TankCost('InvestCostPerCubic');
InvestmentCosts(Scen,'InvestPump')       =  sum((pump,head),PumpCost('InvestCostPump',pump)* x_b_invest_pump.l(pump,head));
InvestmentCosts(Scen,'InvestPV')         =  x_PV_capacity.l *PVcost('InvestCostPerKW');
InvestmentCosts(Scen,'InvestTank')       =  x_tank_capacity.l * TankCost('InvestCostPerCubic');

AnnualCosts(Scen,'TotCost')              = TotCost.l;
AnnualCosts(Scen,'CostPerCubic')         = TotCost.l/sum((h,pump,head),x_pumpflow.l(h,pump,head));
AnnualCosts(Scen,'PumpEAC')              = sum((pump,head),(x_b_invest_pump.l(pump,head) * PumpCost('EAC',pump)));
AnnualCosts(Scen,'PVEAC')                = x_PV_capacity.l*PVcost('EAC');
AnnualCosts(Scen,'TankEAC')              = x_tank_capacity.l * TankCost('EAC');

TechnicalReport(Scen,'PVcap')            = x_PV_capacity.l;
TechnicalReport(Scen,'TankCap')          = x_tank_capacity.l;
TechnicalReport(Scen,'PumpHours')        = sum((h,pump,head),x_b_pump_on.l(h,pump,head));
TechnicalReport(Scen,'PumpHoursPday')    = sum((h,pump,head),x_b_pump_on.l(h,pump,head))/card(d);

report2(Scen,h,'SolarRad')               = solar(h);
report2(Scen,h,'PVpower')                = solar(h) *  x_PV_capacity.l;
report2(Scen,h,'Pmin')                   = sum((pump,head),PumpTech(pump,head,'P_min') * x_b_pump_on.l(h,pump,head));
report2(Scen,h,'PumpPower')              = sum((pump,head),x_pump.l(h,pump,head));
report2(Scen,h,'Pmax')                   = sum((pump,head),PumpTech(pump,head,'P_max') * x_b_pump_on.l(h,pump,head));
report2(Scen,h,'PumpFlow')               = sum((pump,head),x_pumpflow.l(h,pump,head));

DailyFlow(Scen,d,'WaterFlow')            = sum((h,pump,head)$hind(h,d),x_pumpflow.l(h,pump,head));

INVCost=         x_PV_capacity.l *PVcost('InvestCostPerKW')
                 +  sum((pump,head),PumpCost('InvestCostPump',pump)* x_b_invest_pump.l(pump,head))
                 +  x_tank_capacity.l * TankCost('InvestCostPerCubic');

OPcost=          sum((pump,head),PumpCost('InvestCostPump',pump)* x_b_invest_pump.l(pump,head)) * MaintenancePump;


output(scen,t,"ancosts")$(ord(t)<11) = OPcost * ((1+i)**(-(ord(t)-1)));
output(scen,t,"sumcosts") = sum(s$(ord(s)<ord(t)),output(scen,s,"ancosts"));
output(scen,t,"totalcosts")$(ord(t)>1)=INVCost+output(scen,t,"sumcosts");
);


option InvestmentCosts:4:1:1;

Display summary,CostParameters,InvestmentCosts,AnnualCosts,TechnicalReport,report2,DailyFlow,output;


$Onecho > WaterPump_Results_SOLAR.txt
par=summary              rng=Tabelle1!A1         rdim=1 cdim=1
par=CostParameters       rng=Tabelle1!A10        rdim=1 cdim=1
par=InvestmentCosts      rng=Tabelle1!A20        rdim=1 cdim=1
par=AnnualCosts          rng=Tabelle1!A30        rdim=1 cdim=1
par=TechnicalReport      rng=Tabelle1!A40        rdim=1 cdim=1
par=output               rng=Tabelle1!J1         rdim=2 cdim=1
$offecho

execute_unload 'Results_Solar.gdx';
execute 'gdxxrw Results_Solar.gdx output=201904_Results_SOLAR_Scen4_100m.xlsx squeeze=N trace=3 @WaterPump_Results_SOLAR.txt';

