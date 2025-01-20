$TITLE This program process gdx output for model comparison
$setglobal baseyear 2010
$setglobal prog_loc 
$eolcom  !!


*----- Definitions -----*
SET
Model /"MESSAGEix", "AIM"/
Module  /"Income", "Expenditure"/

TH                                                         Poverty threshold/
  "pop_1.9"
  "pop_3.2"
  "pop_5.5"
/

Seg                             Income level in NS HH survey
Y                                                             Year  /1950*2100/
R                                                             Region/
$  include ../%prog_loc%/define/WDIcountry.set
  TWN
  AFR, ASP, CIS, EG2, EG1, OEE, RU2, RU1, SAM, WAS
  "R5OECD90+EU"
  R5ASIA
  R5LAM
  R5MAF
  R5REF
/

Iall    consumption items used in AIDADS /
* $include ../%prog_loc%/define/coicop.set
$include ../%prog_loc%/define/I_Calib.set
  "Housing, water, electricity, gas and other fuels"
	"AGG Food and beverages"
	"AGG Daily, communication and transportation"
	"AGG Education and entertainment"
	"AGG Clothing and footwear"
	"AGG Housing, water, electricity, gas and other fuels"
	"AGG Medical and fitness"
	"AGG Other"
  /

  I(Iall) major 12 items /
$include ../%prog_loc%/define/I_Calib.set
  /

  I_coicop(Iall) /
$include ../%prog_loc%/define/coicop.set
  /

  Idetail prepared items /
$include ../%prog_loc%/define/coicop_detail.set
  /

;

ALIAS(I,J),(I_coicop,J_coicop),(Idetail,Idetail),(R,R2),(Y,Y2),(Seg,seg2);

PARAMETER
Gini(Ref, R, Y, Model, Module)
IncomeCap(Ref, R, Y, Model, Module)
Mu(Ref, R, Y, Model, Module)
Sigma(Ref, R, Y, Model, Module)
Omega(Ref, R, Y, TH, Model, Module)
PoV(Ref, R, Y, TH, Model, Module)
PoVgap_GDPExp(Ref, R, Y, TH, Model, Module)
;

*---- Output ----*

execute_unload '../%prog_loc%/data/ResulsIntegration.gdx'
CFPS
Seg
Y
R
Iall
Iagg
Ins
I
;

$exit
