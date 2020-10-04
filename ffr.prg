' Program for simple simulation with MCE expectations
'
' See the Simulation Basics document for information about
' this program


' *************************************************************
' Initial filename and parameter settings
' *************************************************************

' Subroutines
  include ..\subs\master_library

'must be made available to all programs that run simulations with MC expectations. 
  include ..\subs\mce_solve_library
  include ..\addins\regadd.prg

' Workfile    
  %wfstart = "1975q1"
  %wfend = "2125q4"
  %mainpage = "main"
  wfcreate(wf=aaa,page={%mainpage}) q {%wfstart} {%wfend}

' FRB/US model names and locations
  %varmod = "stdver"
  %mcemod = "pfver"
  %model_path = "..\mods\model.xml"

' Input database
%dbin  = "..\data\longbase"

' MC expectations option ("-mcap","-mcap+wp","-all")
'“-mcap” = only the expectations in asset pricing equations have MC solutions. 

' “-wp” = only the expectations in the price and wage setting equations have MC solutions. 

' “-mcap+wp” = combines the “-mcap” and “-wp” groups. 

  %mcegroup = "-mcap+wp"

' Simulation start and length
  %simstart = "2012q3"
  !nsimqtrs = 16*4


  call dateshift(%simstart,%simend,!nsimqtrs-1)

' Number of quarters to show in graphs
  !graphqtrs = 25


' ****************************************************************
' Retrieve data, model equations and coefficients, set
' policy options, and compute tracking residuals 
' ****************************************************************

' Load equations and coefficients
  read_xml_model(path=%model_path,mce_vars=%mcegroup,mod_f=%mcemod)

' Load data
  dbopen %dbin as longdata
  fetch(d=longdata) *


' Data for extra variables associated with MC expectations

'mce_create_auxillary = subroutine that is called to create data for the variables in the MC simulations. It sets the W variables equal to the Z variables and the E variables to zero. 
  smpl @all
  call mce_create_auxillary(m_zvars)

' Set monetary policy
'dmpintay = inertial taylor rule
'dmptay = taylor rule
  smpl @all
  call set_mp("dmpintay")



'rffmin = exog variable to decide if rff should be subject to ZLB
'if rffmin = 0 or small positie value => impose ZLB
'if rffmin is a large negative # => eliminates ZLB constraint

'rstar = policymaker's estimate of the equm real federal funds rate (ffr)
'dstar = switch variable => use it to control the behavior of rstar
'if dstar = 0 => rstar is exog
'if dstar = 1=> rstar gradually moves towards the simulated value of real ffr

' Turn off zero bound and policy thresholds; hold policymaker's
' perceived equilibrium real interest rate constant for first 10 qts

'Once either threshold is crossed, the endogenous switch variable dmptr becomes 1.0 and the policy rate is determined

 

  
 smpl @all
 dmptrsh = 0 
 rffmin = -9999
 dmptr = 0
 drstar = 1


 smpl %simstart %simstart + 9

  drstar = 0
  smpl @all

' Set fiscal policy

'the trends tax rates adjust to gradually stabilize the ratios of the two government surpluses to GDP at the values given by exogenous variables gfsrt and gssrt  


'if dfpsrp = 1=> the trends adjust to gradually stabilize the ratios of the two government surpluses to GDP at the values given by exogenous variables gfsrt and gssrt 

'selects surplus stabilizing fiscal policy by setting dfpex=0, dfpsrp=1, and dfpdbt=0. 

'When dfpex = 1.0 and the other switches are zero, the trend tax rates are exogenous 

'dfpdbt = 1.0, the trends adjust to gradually stabilize the debt-to-GDP ratios of the each government sector at the values given by the exogenous variables gfdrt and gsdrt. 

  smpl @all
  call set_fp("dfpsrp")

' Set _aerr variables to zero
  smpl @all
  {%varmod}.makegroup(a,n) endog @endog
  call groupnew("endog","_aerr")
  call group2zero("endog_aerr")

'add the formula to create emp to pop ratio in the model.xml add-in file

{%varmod}.append epop = lfpr*(1-lur/100)
{%varmod}.append rgdpch = ((xgdp - xgdp(-4))/ xgdp) * 100


' Standard solution options
  {%varmod}.solveopt(o=b,g=12,z=1e-12)
  {%mcemod}.solveopt(o=b,g=12,z=1e-12)

' Assign baseline tracking add factors
  %suftrk = "_0"
  smpl %simstart %simend 
  {%varmod}.addassign @all
  {%varmod}.addinit(v=n) @all
  {%varmod}.scenario(n,a={%suftrk}) "track"
  {%varmod}.solve
  scalar mm = @max(@abs(xgap{%suftrk}-xgap))
  if mm > .0001 then
    statusline dynamic tracking simulation failed for {%varmod}
    stop
    endif
  {%mcemod}.addassign @all
  {%mcemod}.addinit(v=n) @all

' *************************************************************
' Sim 1:  Monetary Policy Shock using Taylor Rule
' *************************************************************

  %sufsim = "_1"
  {%varmod}.scenario(n,a={%sufsim}) "sim"
  {%mcemod}.scenario(n,a={%sufsim}) "sim"

  smpl %simstart %simstart
  smpl @all
  call set_mp("dmpintay")

'0.5% increase in the federal funds rate prescribed by the intertial Taylor rule
 rffintay_a = rffintay_a + 0.5 


'%simstr = selects the type of simulation. Possible types include single (single) and optimal control (opt, opttc) simulations. 

'%modstr =  contains information about the model 
'=> uses a syntax that requires the name of the full model with :

'1. VAR expectations (mod_b keyword), 
'2. the name of the partial model with MC expectations (mod_f keyword), and
'3.  the names of the expectations variables that are MC (mce_vars keyword )



  %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=m_zvars"
  %algstr = "meth=qnewton"
  %simstr = "type=single"
  smpl %simstart %simend
  call mce_run(%modstr,%algstr,%simstr)

' *************************************************************
' Sim 2:  Monetary Policy Shock using the Inertial Taylor Rule
' *************************************************************
  %sufsim1 = "_2"
  {%varmod}.scenario(n,a={%sufsim1}) "sim1"
  {%mcemod}.scenario(n,a={%sufsim1}) "sim1"

  smpl %simstart %simstart
  smpl @all
  call set_mp("dmptay")

'0.5% increase in the federal funds rate prescribed by the Taylor rule with output gap
 rfftay_a = rfftay_a + 0.5 


  %modstr = "mod_b=%varmod,mod_f=%mcemod,mce_vars=m_zvars"
  %algstr = "meth=qnewton"
  %simstr = "type=single"
  smpl %simstart %simend
  call mce_run(%modstr,%algstr,%simstr)

'***********************************************************
' Make a graph
'***********************************************************

  call dateshift(%simstart,%graphstart,-8)
  call dateshift(%simstart,%graphend, 21)


smpl %graphstart %graphend
  graph fig1a.line rff rff{%sufsim} rff{%sufsim1}
  fig1a.options size(7,4.2)
  fig1a.legend display -inbox position(3.8,2.8) font("arial",15)
  fig1a.datelabel format(yyyy)
  fig1a.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1a.axis(left) font("arial",15)
  fig1a.axis(bottom) font("arial",15)
  fig1a.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1a.setelem(2) lcolor(green)  legend("Taylor") lwidth(1)
  fig1a.setelem(3) lcolor(red)  legend(" Inertial Taylor") lwidth(1)
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
  fig1b.setelem(2) lcolor(green)  legend("Taylor") lwidth(1)
  fig1b.setelem(3) lcolor(red)  legend(" Inertial Taylor") lwidth(1)
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
  fig1c.setelem(2) lcolor(green)  legend("Taylor") lwidth(1)
  fig1c.setelem(3) lcolor(red)  legend(" Inertial Taylor") lwidth(1)
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
  fig1d.setelem(2) lcolor(green)  legend("Taylor") lwidth(1)
  fig1d.setelem(3) lcolor(red)  legend(" Inertial Taylor") lwidth(1)
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
  fig1e.setelem(2) lcolor(green)  legend("Taylor") lwidth(1)
  fig1e.setelem(3) lcolor(red)  legend(" Inertial Taylor") lwidth(1)  
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
  fig1f.setelem(2) lcolor(green)  legend("Taylor") lwidth(1)
  fig1f.setelem(3) lcolor(red)  legend(" Inertial Taylor") lwidth(1)
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
  fig1g.setelem(2) lcolor(green)  legend("Taylor") lwidth(1)
  fig1g.setelem(3) lcolor(red)  legend(" Inertial Taylor") lwidth(1)
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
  fig1h.setelem(2) lcolor(green)  legend("Taylor") lwidth(1)
  fig1h.setelem(3) lcolor(red)  legend(" Inertial Taylor") lwidth(1)
  fig1h.addtext(t,just(c),font("arial",18)) Core PCE Inflation Rate


  %title = " Macroeconomic Effects of an Upward Federal Funds Rate Shock\r"
  if %mcvars_wp = "no" and %mcvars_all = "no" then
    %title = %title + "(MC Expectations in Asset Pricing)"
    endif
  if %mcvars_wp = "yes" and %mcvars_all = "no" then
    %title = %title + "(MC Expectations in Asset Pricing and Price-Wage Setting)"
    endif
  if %mcvars_all = "yes" then
    %title = %title + "(MC Expectations in All Sectors)"
    endif

graph fig1.merge fig1a fig1b fig1c fig1d  fig1e fig1f fig1g fig1h
  fig1.addtext(t,just(c),font("Arial",16)) {%title}

  fig1.addtext(t,just(c),font("Arial",20)) {%title}
  fig1.align(2,1,1.25)
  show fig1


