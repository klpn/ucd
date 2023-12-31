{-# LANGUAGE DeriveGeneric #-}
module Ucrules where

import Caexprs
import Data.Csv
import Data.List
import Data.List.Split
import Text.Regex.TDFA
import GHC.Generics (Generic)

data CodRow = CodRow {sex :: !Char, age :: !Int, uc :: !String, ent :: !String}
    deriving (Generic, Show)

instance FromNamedRecord CodRow
instance ToNamedRecord CodRow
instance DefaultOrdered CodRow

data DeathCert = DeathCert {part1 :: [[String]], part2 :: [String]}
    deriving (Show)

crnew :: CodRow -> CodRow
crnew cr = CodRow{sex = sex cr, age = age cr, uc = ucnew, ent = ent cr}
    where
        ucnew = intercalate "|" $ nub [(uc cr), (ucd . entstrParse) (ent cr)]

ucd dc = ucmods (dc, startUcd $ part1 dc)
ucmods = modCa . illDefCa . obvCa

modCa :: (DeathCert, String) -> String
modCa (dc, tuc)
    | tuc =~ diabNos && length kidDiabs > 0 = modCa (dc, (take 3 tuc) ++ "2")
    | tuc =~ orgNoVd && length neurDegs > 0 = modCa (dc, last neurDegs)
    | tuc =~ alcoPs && length alcoLivs > 0 = modCa (dc, last alcoLivs)
    | tuc =~ tobacco && length chResps > 0 = modCa (dc, last chResps)
    | tuc =~ rhdNos && length chrRhds > 0 = modCa (dc, last chrRhds)
    | tuc == ht && length hhds > 0 = modCa (dc, last hhds)
    | tuc == ht && length hkds > 0 = modCa (dc, last hkds)
    | tuc == ht && length hhkds > 0 = modCa (dc, last hhkds)
    | tuc == ht && length hfs > 0 = modCa (dc, hhdHf)
    | tuc == ht && length hdills > 0 = modCa (dc, hhdOth)
    | tuc == ht && length strs > 0 = modCa (dc, last strs)
    | tuc == ht && length nephrs > 0 = modCa (dc, last nephrs)
    | tuc == ht && length renfs > 0 = modCa (dc, hkdRf)
    | tuc == ht && length kidContrs > 0 = modCa (dc, hkdOth)
    | tuc == hhdHf && length rfAlls > 0 = modCa (dc, hhkdHfRf)
    | tuc == hhdHf && length hkdOthkdills > 0 = modCa (dc, hhkdHfOth)
    | tuc == hhdOth && length hfAlls > 0 = modCa (dc, hhdHf)
    | tuc == hhdOth && length rfAlls > 0 = modCa (dc, hhkdRfOth)
    | tuc == hhdOth && length hkdOthkdills > 0 = modCa (dc, hhkdOth)
    | tuc == hkdRf && length hfAlls > 0 = modCa (dc, hhkdHfRf)
    | tuc == hkdRf && length hhdOthhdills > 0 = modCa (dc, hhkdRfOth)
    | tuc == hkdOth && length rfAlls > 0 = modCa (dc, hkdRf)
    | tuc == hkdOth && length hfAlls > 0 = modCa (dc, hhkdHfOth)
    | tuc == hkdOth && length hhdOthhdills > 0 = modCa (dc, hhkdOth)
    | tuc =~ htd && length ihds > 0 = modCa (dc, last ihds)
    | tuc =~ nonmiihd && length mis > 0 = modCa (dc, last mis)
    | tuc == oldmi = modCa (dc, chrihdOth)
    | tuc == pulmhdNos && length scols > 0 = modCa (dc, scolhd)
    | tuc =~ hdFuncIll && length ihds > 0 = modCa (dc, last ihds)
    | tuc =~ stroke && length vdemTucas > 0 = modCa (dc, last vdemTucas)
    | tuc =~ stroke && length demNosTucas > 0 = modCa (dc, vdemNos)
    | tuc =~ nonIOcclStr = modCa (dc, ceri ++ (snd $ splitAt 3 tuc))
    | tuc =~ cerAth && length strSpecs > 0 = modCa (dc, last strSpecs)
    | tuc =~ cerAth && length pdSecParkTucas > 0 = modCa (dc, vpark)
    | tuc =~ athero && length htds > 0 = modCa (dc, last htds)
    | tuc =~ athero && length ihds > 0 = modCa (dc, last ihds)
    | tuc =~ athero && length hfs > 0 = modCa (dc, last hfs)
    | tuc =~ athero && length hdills > 0 = modCa (dc, last hdills)
    | tuc =~ athero && length strs > 0 = modCa (dc, last strs)
    | tuc =~ athero && length nonrhValvHdTucas > 0 = modCa (dc, last nonrhValvHdTucas)
    | tuc =~ athero && length hdNosTucas > 0 = modCa (dc, athhd)
    | tuc =~ athero && length artOthTucas > 0 = modCa (dc, last artOthTucas)
    | tuc =~ athero && length intvdTucas > 0 = modCa (dc, last intvdTucas)
    | tuc =~ athero && length cnkidContrTucas > 0 = modCa (dc, hkdOth)
    | tuc == atheroGen && length gangrNoss > 0 = modCa (dc, atheroExtr)
    | tuc == atheroGen && length vdemTucas > 0 = modCa (dc, last vdemTucas)
    | tuc == atheroGen && length demNosTucas > 0 = modCa (dc, vdemNos)
    | tuc == atheroGen && length pdSecParkTucas > 0 = modCa (dc, vpark)
    | tuc == taaRup && length aaaRups > 0 = modCa (dc, taaaRup)
    | tuc == taaNoRup && length aaaNoRups > 0 = modCa (dc, taaaNoRup)
    | tuc == aaaRup && length taaRups > 0 = modCa (dc, taaaRup)
    | tuc == aaaNoRup && length taaNoRups > 0 = modCa (dc, taaaNoRup)
    | tuc =~ cold && length imMobs > 0 = modCa (dc, hypoPneum)
    | tuc =~ cold && length fluPneums > 0 = modCa (dc, last fluPneums)
    | tuc =~ cold && length bronchitiss > 0 = modCa (dc, last bronchitiss)
    | tuc =~ pneumOrgNos && length imMobs > 0 = modCa (dc, hypoPneum)
    | tuc =~ othCopd && length pneums > 0 = modCa (dc, infCopd)
    | tuc =~ renf && length hts > 0 = modCa (dc, hkdRf)
    | tuc =~ renf && length hkds > 0 = modCa (dc, hkdRf)
    | tuc =~ inj && length exts > 0 = modCa (dc, last exts)
    | tuc =~ alcoAcc && length drugAccs > 0 = modCa (dc, last drugAccs)
    | tuc =~ nos && length tucNonNoss > 0 = modCa (dc, last tucNonNoss)
    | otherwise = tuc
    where
        p1 = part1 dc 
        p2 = part2 dc 
        p12 = (concat p1) ++ p2
        p12tuca = fst $ break (== tuc) p12
        neurDegs = filter (=~ neurDeg) p12
        chrRhds = filter (=~ chrRhd) p12
        htds = filter (=~ htd) p12
        hts = filter (== ht) p12
        hhds = filter (=~ hhd) p12
        hkds = filter (=~ hkd) p12
        hhkds = filter (=~ hhkd) p12
        ihds = filter (=~ ihd) p12
        mis = filter (=~ mi) p12
        hfs = filter (=~ hf) p12
        hdills = filter (=~ hdIllDef) p12
        strs = filter (=~ stroke) p12
        strSpecs = filter (=~ strokeSpec) p12
        nephrs = filter (=~ nephr) p12
        renfs = filter (=~ renf) p12
        kidContrs = filter (== kidContr) p12
        hkdRfAlls = filter (=~ hkdRfAll) p12
        hkdOthAlls = filter (=~ hkdOthAll) p12
        hkdOthkdills = filter (=~ hkdOthkdill) p12
        hhdHfAlls = filter (=~ hhdHfAll) p12
        hhdOthAlls = filter (=~ hhdOthAll) p12
        hhdOthhdills = filter (=~ hhdOthhdill) p12
        nonrhValvHds = filter (=~ nonrhValvHd) p12
        hfAlls = filter (=~ hfAll) p12
        hdFuncIlls = filter (=~ hdFuncIll) p12
        taaRups = filter (=~ taaRup) p12
        taaNoRups = filter (=~ taaNoRup) p12
        aaaRups = filter (=~ aaaRup) p12
        aaaNoRups = filter (=~ aaaNoRup) p12
        fluPneums = filter (=~ fluPneum) p12
        pneums = filter (=~ pneum) p12
        bronchitiss = filter (=~ bronchitis) p12
        chResps = filter (=~ chResp) p12
        alcoLivs = filter (=~ alcoLiv) p12
        scols = filter (=~ scol) p12
        rfAlls = filter (=~ rfAll) p12
        kidDiabs = filter (=~ kidDiab) p12
        gangrNoss = filter (=~ gangrNos) p12
        imMobs = filter (== imMob) p12
        exts = filter (=~ ext) p12
        drugAccs = filter (=~ drugAcc) p12
        tucNonNoss = filter (=~ (take 3 tuc ++ "[0-8]")) p12
        vdemTucas = filter (=~ vdem) p12tuca
        demNosTucas = filter (=~ demNos) p12tuca
        pdSecParkTucas = filter (=~ pdSecPark) p12tuca
        nonrhValvHdTucas = filter (=~ nonrhValvHd) p12tuca
        hdNosTucas = filter (=~ hdNos) p12tuca
        artOthTucas = filter (=~ artOth) p12tuca
        intvdTucas = filter (=~ intvd) p12tuca
        cnkidContrTucas = filter (=~ cnkidContr) p12tuca

illDefCa :: (DeathCert, String) -> (DeathCert, String)
illDefCa (dc, tuc) 
    | tuc =~ illDef = obvCa (dc, startUcd $ removeIllDef dc)
    | otherwise = (dc, tuc)

removeIllDef :: DeathCert -> [[String]]
removeIllDef dc
    | length nonIllDefsP1 > 0 = nonIllDefsP1
    | length nonIllDefsP2 > 0 = [nonIllDefsP2]
    | otherwise = p1
    where
        p1 = part1 dc 
        p2 = part2 dc 
        nonIllDefsP1 = filter (/= []) (filter notIllDef <$> p1)
        nonIllDefsP2 = filter notIllDef p2

notIllDef :: String -> Bool
notIllDef c = (c =~ rNonIllDef) || (not $ c=~ illDef)

obvCa :: (DeathCert, String) -> (DeathCert, String)
obvCa (dc, tuc) 
    | tuc =~ obvConsHiv && length hivs > 0 = obvCa (dc, last hivs)
    | tuc =~ pneum && length pcas > 0 = obvCa (dc, last pcas)
    | tuc =~ hfHdNos && length othSpecHds > 0 = obvCa (dc, last othSpecHds)
    | otherwise = (dc, tuc)
    where
        p1 = part1 dc 
        p2 = part2 dc 
        p12 = (snd $ break (== tuc) $ (concat p1) ++ p2)
        hivs = filter (=~ hiv) p12
        pcas = filter (=~ pneumObvCa) p12
        othSpecHds = filter (=~ othSpecHd) p12

startUcd :: [[String]] -> String
startUcd p1
    | length p1 == 1 = head $ head p1
    | allExpl p1 == [True] = head $ last p1
    | length aseqs > 0 = last $ head aseqs
    | otherwise = startUcd $ init p1
    where aseqs = filter allowedSeq (sequence p1)

allExpl :: [[String]] -> [Bool]
allExpl p1 = nub $ allowedSeq <$> (\l -> [l] ++ [head $ last p1]) <$> (concat $ init p1)

allowedSeq :: [String] -> Bool
allowedSeq p1
    | length p1 == 1 = True
    | (length $ snd $ break (=~ prohibCons) p1) > 1 = False
    | (length achs) > 1 && not(head (tail achs) =~ hiv) = False
    | (length aiis) > 1 && not(head (tail aiis) =~ immuneImp) = False
    | (length rhds) > 1 && not(head (tail rhds) =~ strepDis) = False
    | (length pcncs) > 1 && head (tail pcncs) =~ neoplasms = False
    | (length falls) > 1 && not(head (tail falls) =~ osteoDis) = False
    | otherwise = True
    where
        achs = snd $ break (=~ allowedCancHiv) p1
        aiis = snd $ break (=~ allowedImmuneImp) p1
        rhds = snd $ break (=~ rheumHd) p1
        pcncs = snd $ break (=~ pcNeoCvd) p1
        falls = snd $ break (=~ fallAcc) p1

entstrParse :: String -> DeathCert
entstrParse es 
    | p1 == [] = DeathCert{part1 = [p2], part2 = []}
    | otherwise = DeathCert{part1 = p1, part2 = p2}
    where
        esw = takeWhile (/= ' ') <$> chunksOf 7 es
        p1con = filter (\c -> head c /= '6') esw
        p1 = (fmap $ drop 2) <$> (groupBy (\a b -> head a == head b) p1con)
        p2 = drop 2 <$> filter (\c -> head c == '6') esw
