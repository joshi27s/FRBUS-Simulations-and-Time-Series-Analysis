' Program for simulation under VAR expectations that illustrates how
' to set the monetary policy options that impose the zero lower bound
' on the funds rate and delay the liftoff of the funds rate from the
' ZLB until either the unemployment rate falls below a threshold or
' inflation rises above a threshold.
'
' See FRB/US Simulation Basics document for general information about
' this program.

' Additional notes:

' 1. The scenario involves a set of negative aggregate demand 
' shocks and a positive risk premium shock that start in 2003q3,
' when the baseline (historical) funds rate is about one percent.
' The shocks are equal to the equation errors actually observed
' in the four quarters starting in 2008q4.

' 2. To impose the ZLB set %zb = "yes" (rather than "no")

' 3. To impose the policy liftfoff threshold conditions set both
' %zb = "yes" and %threshold = "yes".  For illustrative purposes
' and reflecting the baseline conditions in 2003 and the years
' that immediately follow, the inflation threshold is set to 3.0
' and the unemployment threshold is set to 7.0, subject to the
' the adjustments described next.

' 4. Because the threshold conditions only make sense once the ZLB is
' binding, unemployment is above its threshold level (lurtrsh),
' and inflation is below its threshold (pitrsh), which is not the
' case in the initial simulation quarters, the program turns on the
' threshold code (using dmptrsh) in the 5th simulation quarter,
' at which point these conditions hold. In addition, for the threshold 
' code to work properly, the endogenous switch variable dmptr must be 
' zero in the quarter prior to the quarter in which the threshold code is 
' turned on.  This is accomplished by setting the baseline data on dmptr 
' to zero and by setting the unemployment and inflation thresholds
' (lurtrsh, pitrsh) to values in the first four simulation quarters that
' would not flip the dmptr switch to one. 

' 4. Choose one of the five available policy rules by setting
' %policy to one of rffintay, rfftay, rfftlr, rffalt, or rffgen.

' 5. If neither the ZLB or thresholds are imposed, the monetary policy
' equations have baseline-tracking adds and the simulation is
' a standard deviations-from-baseline exercise. 

' 6. If either the ZLB or thresholds are imposed, the add factors on 
' monetary policy equations are set to zero after the tracking adds
' are computed so that the ZLB and threshold conditions are based on the
' actual simulated outcomes for the funds rate and inflation and unemployment,
' not their deviations from baseline. 

' *************************************************************
' Initial filename and parameter settings
' *************************************************************

' Subroutines

  'master_library must be made available to all simulations programs 
  include ..\subs\master_library

' Workfile    
  %wfstart = "1975q1"
  %wfend = "2125q4"
  %mainpage = "main"

'noerr: Do not error if the object doesn’t exist.
  wfclose(noerr)
  wfcreate(wf=aaa,page={%mainpage}) q {%wfstart} {%wfend}


' FRB/US MODEL NAMES AND LOCATIONS

'loads all FRB/US non-expectations equations and the VAR-based formulas for all expectations variables into a single model. The model is automatically named stdver: 

'model = consists of a set of equations that describe the relationships between a set of variables.

'Any variable that is not assigned as the endogenous variable for any equation is considered exogenous to the model.


'stdver = 1st model that contains all FRB/US non-expectations equations and the VAR-based formulas for all expectations equations
  %varmod = "stdver"

'model.xml stores the FRB/US equations and coefficients

'The provided programs use the read_xml_model add-in command to load this information into EViews. 
  %model_path = "..\mods\model.xml"

' Input database
%dbin  = "..\data\longbase"

' Simulation start and length
  %simstart = "2020q2"
  !nsimqtrs = 16*4  
 call dateshift(%simstart,%simend,!nsimqtrs-1)

' MONETARY POLICY

' Choose a monetary policy rule from the given 5 policy reaction functions and 2 exogenous policies that hold the nominal or real funds rate at a predetermined path

'Examples of different monetary policy reaction functions are: 
'rffintay = Intertial Taylor rule
'rfftay = Taylor rule with output gap
'rfftlr = Taylor rule with unemployment gap

'The monetary policy equations for rffintay, rfftay, rfftlr, and rffgen contain the variable rstar, which can be interpreted as the policymakers’ estimate of the equilibrium real funds rate. 

  %zb = "yes"
  %threshold = "yes"
  %policy = "rfftay"


' ****************************************************************
' Retrieve data, model equations and coefficients, set
' policy options, and compute tracking residuals 
' ****************************************************************

' Load equations and coefficients
  read_xml_model(path=%model_path)

' LOAD DATA

'smpl allows us to conduct statistical operations and series assignment expressions in the  workfile sample
'@all - entire workfile range
  dbopen %dbin as longbase
  smpl @all
  fetch(d=longbase) *

  smpl @all

' SET MONETARY POLICY RULE

' Set monetary policy to use the first-difference policy rule (coded as rffgen)

'dmpgen = one of the 7 seven options available to set the federal funds rate (rff)
'Its reaction function equation is 'rffgen'
'dmpgen = exogenous switch variable that takes value of 0 or 1.

'dmpalt = reaction function based on estimated rule : Monetary policy switch: MA rule

'dmptay = Taylor rule reaction function
'dmpintay = inertial taylor rule

 %policydmp = @replace(%policy,"rff","dmp")
  call set_mp(%policydmp)

'rff= federal funds rate
'rffmin = exog switch variable to decide if rff should be subject to ZLB: takes value 0 or 1
'if rffmin = 0 or small positie value => impose ZLB
'if rffmin is a large negative # => eliminates ZLB constraint

'rstar = policymaker's estimate of the equm real federal funds rate (ffr)
'dstar = switch variable => use it to control the behavior of rstar
'if dstar = 0 => rstar is exog
'if dstar = 1=> rstar gradually moves towards the simulated value of real ffr

'Because drstar is a timeseries, the value of rstar may be fixed during part of a simulation and allowed to vary endogenously during the other part. 

' Set ZLB
  if %zb = "yes" then
    rffmin = .125
    else
    rffmin = -9999
    endif

' Set threshold variables 
  if %threshold = "yes" then
    if %zb = "no" then
      @uiprompt("When policy thresholds are imposed, the zero bound must also be imposed")
      stop
      endif
    smpl @all
    call dateshift(%simstart,%quarter4,3)

'dmptr = exog switch variable which is 1, once either threshold is crossed,
'=>and the policy rate is determined 

'dmptrsh = exog switch variable
'if dmptrsh = 1, delay the liftoff from ZLB until either the unemployment rate falls below a critical rate (lurtrsh) or expected inflation rises above a critical rate (pitrsh).

  ' thresholds (dmptrsh and dmptr) not active in first 4 qtrs
   smpl %simstart - 1 %quarter4
    dmptrsh = 0
    lurtrsh = -9999
    pitrsh = 9999
    dmptr = 0

  ' thresholds (dmptrsh and dmptr) active starting in qtr 5
    smpl %quarter4 + 1 %simend
    dmptrsh = 1
    lurtrsh = 6.5
    pitrsh = 2.5
 

    smpl @all
    else
    smpl @all
    dmptrsh = 0
    endif

  smpl @all
  drstar = 0

' SET FISCAL POLICY

'if dfpsrp = 1=> the trends adjust to gradually stabilize the ratios of the two government surpluses to GDP at the values given by exogenous variables gfsrt and gssrt 

'we are measuring the behavior of the trend federal and state and local personal income tax rates 

'gfsrt	= Federal government target surplus-to-GDP ratio

'we can select the fiscal policy that stabalizes surplus by setting dfpex=0, dfpsrp=1, and dfpdbt=0. 

'When dfpex = 1.0 and the other switches are zero, the trend tax rates are exogenous 
  smpl @all
  call set_fp("dfpsrp")


'ADD FACTORS 

' We assign add factor variables (addassign) to all the equations in all program examples and initialize the factor variables' values (addint) s.t. when there is no shock, simulated outcomes for all endogenous variables replicate the baseline database. 


'Each FRB/US equation also comes coded with a separate add factor variable whose name contains _aerr as a suffix 

' SET _aerr VARIABLES TO ZERO
  smpl @all
  {%varmod}.makegroup(a,n) endog @endog
  call groupnew("endog","_aerr")
  call group2zero("endog_aerr")


'%varmod = is a string => previously defined name of a FRB/US version.
'Add the data on epop and related thresholds
{%varmod}.append rgdpch = ((xgdp - xgdp(-4))/ xgdp) * 100
{%varmod}.append epop = lfpr*(1-lur/100)




' STANDARD SOLUTION OPTIONS

'The mechanics of running a FRB/US simulation with VAR expectations are different from those associated with MC expectations options.

'solveopt = sets options for model solution but does not solve the model. 
'o = Solution method: “g” (Gauss-Seidel), “n” (Newton), “b” (Broyden).
'g = Number of digits to round solution: an integer value (number of digits)
'z= Zero value: a positive number below which the solution (absolute value) is set to zero

'FRB/US simulations that use the EViews quasi-Newton Broyden algorithm (o=b) usually converge successfully 

'If the algorithms don't converge, especially if a “solver stalled” error message occurs, we should switch to the EViews Newton (o=n) or Gauss-Seidel (o=g) 

' Standard solution options
  {%varmod}.solveopt(o=b,g=12,z=1e-12)


' ASSIGN BASELINE TRACKING ADD FACTORS

'addassign = assign add factors to the equation

'add factor = extra exogenous variable which is included in the selected equation in a particular way.

'if model is solved stochastically, we add random errors  to each equation, but the random errors are still chosen so that their average value is zero.

' If we have additional information about the type of errors that are likely during our forecast period, then we may incorporate that information into the model using add factors.

'The add factors smoothen the transition from historical data into the forecast period. 

'They also compensate for the equation(s)' poor fit in the model near the end of the historical data, if we think it will persist into the forecast time horizon.





  %suftrk = "_0"
  smpl %simstart 2020q2

'@all = assign add factors to all equations in varmod
  {%varmod}.addassign @all

' initialize add factors to equations in varmod via addint
'v=n: set the values of the add factor so that the equation is exactly satisfied without error when the variables of the model are set to the values contained in the actual series (typically the historical data).

  {%varmod}.addinit(v=n) @all
  {%varmod}.scenario(n,a={%suftrk}) "track"

''solve =  finds the solution to a simultaneous equation model for the set of observations specified in the current workfile sample.

'=> solve the model = for a given set of values of the exogenous variables, X, we will try to find a set of values for the endogenous variables, Y

'solves the "%varmod" model . It is a single model that contains non-expectations equations and the VAR-based formulas for all expectations variables 
  {%varmod}.solve

''scalar = holds a single numeric value.
  scalar mm = @max(@abs(xgap{%suftrk}-xgap))
  if mm > .0001 then
    statusline dynamic tracking simulation failed for {%varmod}
    stop
    endif

'endif = marks the end of an IF, or an IF-ELSE statement.


' Set monetary policy add factors to zero when ZLB or threshold are
' imposed

  if %zb = "yes" then
    smpl @all
    {%policy}_a = 0
    rffrule_a = 0
    rff_a = 0
    if %threshold = "yes" then
      dmptpi_a = 0
      dmptlur_a = 0
      dmptmax_a = 0
      dmptr_a = 0
      endif
    endif




' *************************************************************************************************
' Sim 1: Negative AD Shock and  Taylor rule after crossing pitrsh
' *************************************************************************************************
'n = creates a new scenario with a specified name

'a = Set the scenario alias string to be used when creating aliased variables 
'=>string = 1 to 3 alphanumeric string to be used in creating aliased variables. 
'If an underscore is not specified, one will be added to the beginning of the string.


'creates a senario called "sim" and associates it with the alias "_1"

'scenario = we can examine simulation results through models. These models are based on different assumptions about the variables that are outside the model. These assumptions are called scenarios. 

  %sufsim = "_1"
  {%varmod}.scenario(n,a={%sufsim}) "sim"


  smpl @all
  call set_mp("dmptay")
  %policy = "rfftay"


' Set fiscal policy
  smpl @all
call set_fp("dfpsrp")


smpl %simstart %simstart

' shock values are taken from equation residuals for 2008q4-2009q3
  eco_a.fill(o=%simstart) -.006, -.006, -.011, -.001
  ecd_a.fill(o=%simstart) -.091, -.018, -.021,  .029
  eh_a.fill(o=%simstart)  -.076, -.078, -.040,  .073
  ebfi_a.fill(o=%simstart) -.114, -.108,  .050,  .049
  rbbbp_a.fill(o=%simstart) 2.70, 0.38, -0.89, -1.35


 dmptrsh = 1
dmptmax = 1 
 rffmin = 0.0 ' impose ZLB
 drstar = 1 'rstar is exogenous
 pitrsh = 2.5
 lurtrsh = 6.5
 dmptr = 1

'need to exercise caution when using either the ZLB or the liftoff thresholds in simulations that impose baseline-tracking add factors. 

'If the add factors on various key equations (inc that of rff) in the monetary sector are not  zero => there may be unintended effects of the ZLB constraint and the threshold conditions. 

  smpl %simstart %simend
  {%varmod}.solve


' *************************************************************************************************
' Sim 2 : Negative AD Shock and Taylor rule after crossing ecitrsh
' *************************************************************************************************

  %sufsim1 = "_2"
  {%varmod}.scenario(n,a={%sufsim1}) "sim2"
  %policy = "rfftay"

  smpl @all
  call set_mp("dmptay")

' Set fiscal policy
  smpl @all
  call set_fp("dfpex")


smpl %simstart %simstart


' shock values are taken from equation residuals for 2008q4-2009q3
  eco_a.fill(o=%simstart) -.006, -.006, -.011, -.001
  ecd_a.fill(o=%simstart) -.091, -.018, -.021,  .029
  eh_a.fill(o=%simstart)  -.076, -.078, -.040,  .073
  ebfi_a.fill(o=%simstart) -.114, -.108,  .050,  .049
  rbbbp_a.fill(o=%simstart) 2.70, 0.38, -0.89, -1.35

'In order to establish thresholds of eciwag_rate In the model, I had to alter the existing equations and add a few new variables. 

'The variables that affect the threshold-based policies are dmptlur and  dmptpi-the monetary policy indicators of unemployment and PCE inflation rates, respectively. 
'I replaced dmptpi with  dmpteci-the monetary policy indicators of the ECI wage rates. 

'dmpteci =1 delays the liftoff of the federal funds rate from the ZLB until either unemployment rate falls below the critical rate (lurtrsh) , or eciwag_rate rises above the threshold (ecitrsh)



' Set threshold variables 
  if %threshold = "yes" then
    if %zb = "no" then
      @uiprompt("When policy thresholds are imposed, the zero bound must also be imposed")
      stop
      endif
    smpl @all
    call dateshift(%simstart,%quarter4,3)

  ' thresholds (dmptrsh and dmptr) not active in first 4 qtrs
   smpl %simstart - 1 %quarter4
    dmptrsh = 0
    lurtrsh = -9999
    pitrsh = 9999
    dmptr = 0

  ' thresholds (dmptrsh and dmptr) active starting in qtr 5
   smpl %quarter4 + 1 %simend
    dmptrsh = 1
    dmptmax = 1 
    rffmin = 0.0 ' impose ZLB
    lurtrsh = 6.5
    ecitrsh = 3.5
   dmpteci = 1


    smpl @all
    else
    smpl @all
    dmptrsh = 0
    endif


' Set monetary policy add factors to zero when ZLB or threshold are
' imposed

  if %zb = "yes" then
    smpl @all
    {%policy}_a = 0
    rffrule_a = 0
    rff_a = 0
    if %threshold = "yes" then
       dmpteci_a = 0
      dmptmax_a = 0
      dmptr_a = 0
      endif
    endif

'These alterations (from including ECI wage rate threshold) will also affect the policy rule for calculating the federal funds rate, the various monetary policy reaction functions such as the Taylor and the inertial Taylor rule. 

'Adding these variables will affect dmptrsh - switch variables for monetary policy. 

'These are the endogenous trigger variables that get affected:

'dmpmax=1 when either lurtrsh or ecitrsh is breached
{%varmod}.drop dmptmax 
{%varmod}.append dmptmax = @pmax(dmpteci, dmptlur)

'dmptr is initially 0 and remains at that value until either one of the thresholds is crossed, after which it equals to 1. 
{%varmod}.drop dmptr
{%varmod}.append dmptr = @pmax(dmptmax, dmptr(-1))

'changes in the aforementioned will affect the calculation of the real federal funds rate below:
{%varmod}.drop rff
{%varmod}.append rff =(1-dmptrsh) * (@pmax((rffrule),( rffmin))) + dmptrsh * (@pmax(((dmptr(-1)*rffrule +(1-dmptr(-1))*rffmin)),( rffmin)))



  smpl %simstart %simend
  {%varmod}.solve



'***********************************************************
' Make a graph
'***********************************************************

  call dateshift(%simstart,%graphstart,-5)
  call dateshift(%simstart,%graphend, 14)


smpl %graphstart %graphend
  graph fig1a.line rff rff{%sufsim} rff{%sufsim1}
  fig1a.options size(7,4.2)
  fig1a.legend display -inbox position(3.8,2.8) font("arial",15)
  fig1a.datelabel format(yyyy)
  fig1a.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1a.axis(left) font("arial",15)
  fig1a.axis(bottom) font("arial",15)
  fig1a.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1a.setelem(2) lcolor(green)  legend("PCE rate = 2.5%") lwidth(1)
 fig1a.setelem(3) lcolor(red)  legend(" ECI rate = 3.5%") lwidth(1)
  fig1a.addtext(t,just(c),font("arial",18)) Federal Funds Rate


  smpl %graphstart %graphend
  graph fig1b.line rgdpch rgdpch{%sufsim} rgdpch{%sufsim1}
  fig1b.options size(7,4.2)
  fig1b.legend display -inbox position(3.8,2.8) font("arial",15)
  fig1b.datelabel format(yyyy)
  fig1b.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1b.axis(left) font("arial",15)
  fig1b.axis(bottom) font("arial",15)
  fig1b.setelem(1) lcolor(black) legend("Consensus baseline") lwidth(1)
  fig1b.setelem(2) lcolor(green)  legend("PCE rate = 2.5%") lwidth(1)
  fig1b.setelem(3) lcolor(red)  legend("ECI rate = 3.5%") lwidth(1)
  fig1b.addtext(t,just(c),font("arial",18)) Year to Year % Change in Real GDP


  smpl %graphstart %graphend
  graph fig1c.line  lur lur{%sufsim} lur{%sufsim1}
  fig1c.options size(7,4.2)
  fig1c.legend display -inbox position(3.9,0.3) font("arial",15)
  fig1c.datelabel format(yyyy)
  fig1c.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1c.axis(left) font("arial",15)
  fig1c.axis(bottom) font("arial",15)
  fig1c.setelem(1) lcolor(black) legend("Consensus baseline") lwidth(1)
  fig1c.setelem(2) lcolor(green)  legend("PCE rate = 2.5%") lwidth(1)
  fig1c.setelem(3) lcolor(red)  legend("ECI rate = 3.5%") lwidth(1)
  fig1c.addtext(t,just(c),font("arial",18)) Unemployment Rate


  smpl %graphstart %graphend
  graph fig1d.line pic4 pic4{%sufsim} pic4{%sufsim1}
  fig1d.options size(7,4.2)
  fig1d.legend display -inbox position(0.5,0.2) font("arial",15)
  fig1d.datelabel format(yyyy)
  fig1d.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1d.axis(left) font("arial",15)
  fig1d.axis(bottom) font("arial",15)
  fig1d.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1d.setelem(2) lcolor(green)  legend("PCE rate = 2.5%") lwidth(1)
 fig1d.setelem(3) lcolor(red)  legend("ECI rate = 3.5%") lwidth(1)
 fig1d.addtext(t,just(c),font("arial",18)) PCE Inflation Rate (4-Quarter)

smpl %graphstart %graphend
  graph fig1e.line  epop epop{%sufsim} epop{%sufsim1}
  fig1e.options size(7,4.2)
  fig1e.legend display -inbox position(3.9,0.3) font("arial",15)
  fig1e.datelabel format(yyyy)
  fig1e.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1e.axis(left) font("arial",15)
  fig1e.axis(bottom) font("arial",15)
  fig1e.setelem(1) lcolor(black) legend("Consensus baseline") lwidth(1)
  fig1e.setelem(2) lcolor(green)  legend("PCE rate = 2.5%") lwidth(1)
  fig1e.setelem(3) lcolor(red)  legend("ECI rate = 3.5%") lwidth(1)
   fig1e.addtext(t,just(c),font("arial",18)) Employment to Population Ratio

  smpl %graphstart %graphend
  graph fig1f.line pieci pieci{%sufsim} pieci{%sufsim1}
  fig1f.options size(7,4.2)
  fig1f.legend display -inbox position(0.5,0.2) font("arial",15)
  fig1f.datelabel format(yyyy)
  fig1f.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1f.axis(left) font("arial",15)
  fig1f.axis(bottom) font("arial",15)
  fig1f.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1f.setelem(2) lcolor(green)  legend("PCE rate = 2.5%") lwidth(1)
  fig1f.setelem(3) lcolor(red)  legend("ECI rate = 3.5%") lwidth(1)
  fig1f.addtext(t,just(c),font("arial",18)) Annualized rate of growth of EI hourly compensation

smpl %graphstart %graphend
  graph fig1g.line rg10 rg10{%sufsim} rg10{%sufsim1}
  fig1g.options size(7,4.2)
  fig1g.legend display -inbox position(0.5,0.2) font("arial",15)
  fig1g.datelabel format(yyyy)
  fig1g.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1g.axis(left) font("arial",15)
  fig1g.axis(bottom) font("arial",15)
  fig1g.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1g.setelem(2) lcolor(green)  legend("PCE rate = 2.5%") lwidth(1)
  fig1g.setelem(3) lcolor(red)  legend("ECI rate = 3.5%") lwidth(1)
  fig1g.addtext(t,just(c),font("arial",18)) 10-Year Treasury Rate

smpl %graphstart %graphend
  graph fig1h.line picxfe picxfe{%sufsim} picxfe{%sufsim1}
  fig1h.options size(7,4.2)
  fig1h.legend display -inbox position(0.5,0.2) font("arial",15)
  fig1h.datelabel format(yyyy)
  fig1h.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1h.axis(left) font("arial",15)
  fig1h.axis(bottom) font("arial",15)
  fig1h.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1h.setelem(2) lcolor(green)  legend("PCE rate = 2.5%") lwidth(1)
  fig1h.setelem(3) lcolor(red)  legend("ECI rate = 3.5%") lwidth(1)
  fig1h.addtext(t,just(c),font("arial",18)) Core PCE Inflation Rate




  %title = "Macroeconomic Effects of a Negative Aggregate Demand Shock\r(VAR Expectations"
  %title = %title + "; Policy = " + %policy + ")"
  if %zb = "yes" and %threshold = "no" then
    %title = %title + "\r(ZLB Imposed)"
    endif
  if %zb = "yes" and %threshold = "yes" then
    %title = %title + "\r(ZLB and Thresholds Imposed)"
    endif

  graph fig1.merge fig1a fig1b fig1c fig1d  fig1e fig1f fig1g fig1h
  fig1.addtext(t,just(c),font("Arial",16)) {%title}


 fig1.addtext(t,just(c),font("Arial",20)) {%title}
  fig1.align(2,1,1.25)
  show fig1


