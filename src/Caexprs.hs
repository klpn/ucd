module Caexprs where

import Data.List

pcInfA = "A(0[05]|2[0-37]|3[03-9]|7[015-9]|8[0-6]|9[25-8])"
allowedImmuneImp = "A(0[123]|1[5-9])|B90"
strepDis = "A(38|40)|J(0[23])"
hivConsA = "A(0[^05]|2[^0-37]|3[12]|[456]|72|8[789]|9[16]])"
pcInfB = "B(0[3-6]|1[6-8]|26|5[07]|9[124])"
hivConsB = "B(0[^3-6]|1[^6-8]|2[^6]|[3468]|5[^07]|9[^124])"
immuneImp = "B2[0-4]|[CD]"
hiv = "B2[0-4]"
neoplasms = "C|D[0-4]"
pcNeo = "C(1[1-9]|2[02-9]|3|4[^6]|5[04-9]|6[1-9]|8[07-9])|D([0-4]|6[678])"
allowedCancHiv = "C(10|21|46|5[123]|60|8[1-6])"
hivConsNeo = "C(46|53|83)|D8"
stroke = "F01|I6"
neurdegnovd = "F0[3-9]|G[1-3]"
rheumHd = "I0"
htd = "I1"
ht = "I10"
hhd = "I11"
hhdhf = "I110"
hhdoth = "I119"
hkd = "I12"
hkdrf = "I120"
hkdoth = "I129"
hhkd = "I13"
hhkdhfoth = "I130"
hhkdrfoth = "I131"
hhkdhfrf = "I132"
hhkdoth = "I139"
hkdrfall = intercalate "|" [hkdrf, hhkdrfoth, hhkdhfrf]
hkdothall = intercalate "|" [hkdoth, hhkdoth, hhkdhfoth]
hhdhfall = intercalate "|" [hhdhf, hhkdhfoth, hhkdhfrf]
hhdothall = intercalate "|" [hhdoth, hhkdoth, hhkdrfoth]
hhdothhdill = intercalate "|" [hhdothall, hdilldef]
hkdothkdill = intercalate "|" [hkdothall, kidcontr]
hfall = intercalate "|" [hhdhfall, hf]
rfall = intercalate "|" [hkdrfall, renf]
ihd = "I2[0-5]"
nonmiihd = "I2[^1]"
chrihd = "I(2[05])"
mi = "I21"
oldmi = "I252"
chrihdoth = "I258"
scolhd = "I271"
pulmhdnos = "I279"
conddis = "I4[4-5]"
arryth = "I4[6-9]"
hf = "I50"
hdilldef = "I51[4-9]"
hdfuncill = intercalate "|" [conddis, arryth, hf, hdilldef]
athero = "I70"
flu = "J(09|1[01])"
pneum = "J1[2-8]"
scol = "M41"
osteoDis = "M8[0-5]"
nephr = "N0[013-5]"
renf = "N1[7-9]"
kidcontr = "N26"
inj = "[ST]"
sarscov = "U0[47]"
ext = "[V-Y]"
pcExt = "V|W([2-69]|7[0-7]|8[1-9])|X|Y"
fallAcc = "W[01]"
obvConsHiv = intercalate "|" [hivConsA, hivConsB, hivConsNeo, pneum]
pcNeoCvd = intercalate "|" [chrihd, athero]
prohibCons = intercalate "|" [pcInfA, pcInfB, pcNeo, flu, pcExt, sarscov] 
pneumObvCa = intercalate "|" [neurdegnovd, stroke]