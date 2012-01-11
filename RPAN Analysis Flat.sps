GET DATA /TYPE=XLSX 
  /FILE='T:\files\docs\Research\SUN2\data and analysis\SUN2 Data 2009-07-28.xlsx' 
  /SHEET=name 'Flat' 
  /CELLRANGE=full 
  /READNAMES=on 
  /ASSUMEDSTRWIDTH=32767. 
DATASET NAME Flat WINDOW=FRONT.

RECODE Condition ('BSSS'=0) ('BSST'=1) ('BTSS'=2) ('BTST'=3) INTO CondCode.
RECODE Condition ('BSSS'=0) ('BSST'=0) ('BTSS'=1) ('BTST'=1) INTO BT.
RECODE Condition ('BSSS'=0) ('BSST'=1) ('BTSS'=0) ('BTST'=1) INTO ST.
VARIABLE LABELS  CondCode 'Condition Code'. 
VARIABLE LABELS  BT 'Buyer Talks'. 
VARIABLE LABELS  ST 'Seller Talks'. 
EXECUTE.

 **Exclude questionable cases ** 
USE ALL. 
COMPUTE filter_$=(Potential_Exclude < 1). 
VARIABLE LABEL filter_$ 'Potential_Exclude < 1 (FILTER)'. 
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'. 
FORMAT filter_$ (f1.0). 
FILTER BY filter_$. 
EXECUTE.

**Own RP**
UNIANOVA OwnRP BY Buyer BT ST 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Buyer BT ST Buyer*BT Buyer*ST BT*ST Buyer*BT*ST.

UNIANOVA OwnRP BY Buyer BT ST 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /CRITERIA=ALPHA(0.05) 
  /DESIGN=Buyer BT ST.

**Retail Sum**
UNIANOVA retail_sum BY Buyer BT ST 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Buyer BT ST Buyer*BT Buyer*ST BT*ST Buyer*BT*ST.

UNIANOVA retail_sum BY Buyer BT ST 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Buyer BT ST.

**Average Price**
UNIANOVA AvgPrice BY Buyer BT ST 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Buyer BT ST Buyer*BT Buyer*ST BT*ST Buyer*BT*ST.

UNIANOVA AvgPrice BY Buyer BT ST 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=EXCLUDE 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=Buyer BT ST.

USE ALL. 
COMPUTE filter_$=(Buyer = 1). 
VARIABLE LABEL filter_$ 'Buyer = 1 (FILTER)'. 
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'. 
FORMAT filter_$ (f1.0). 
FILTER BY filter_$. 
EXECUTE. 

T-TEST PAIRS=OwnRP WITH OtherRP (PAIRED) 
  /CRITERIA=CI(.9500) 
  /MISSING=ANALYSIS.

