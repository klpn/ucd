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
ucmods = modCa . obvCa

modCa :: (DeathCert, String) -> String
modCa (dc, tuc)
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
    | tuc == hhdOth && length rfAlls > 0 = modCa (dc, hhkdRfOth)
    | tuc == hhdOth && length hkdOthkdills > 0 = modCa (dc, hhkdOth)
    | tuc == hkdRf && length hfAlls > 0 = modCa (dc, hhkdHfRf)
    | tuc == hkdRf && length hhdOthhdills > 0 = modCa (dc, hhkdRfOth)
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
    | tuc =~ inj && length exts > 0 = modCa (dc, last exts)
    | otherwise = tuc
    where
        p1 = part1 dc 
        p2 = part2 dc 
        p12 = (concat p1) ++ p2
        p12tuca = fst $ break (== tuc) p12
        chrRhds = filter (=~ chrRhd) p12
        htds = filter (=~ htd) p12
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
        rfAlls = filter (=~ rfAll) p12
        scols = filter (=~ scol) p12
        gangrNoss = filter (=~ gangrNos) p12
        exts = filter (=~ ext) p12
        vdemTucas = filter (=~ vdem) p12tuca
        demNosTucas = filter (=~ demNos) p12tuca
        pdSecParkTucas = filter (=~ pdSecPark) p12tuca
        nonrhValvHdTucas = filter (=~ nonrhValvHd) p12tuca
        hdNosTucas = filter (=~ hdNos) p12tuca
        artOthTucas = filter (=~ artOth) p12tuca
        intvdTucas = filter (=~ intvd) p12tuca
        cnkidContrTucas = filter (=~ cnkidContr) p12tuca

obvCa :: (DeathCert, String) -> (DeathCert, String)
obvCa (dc, tuc) 
    | tuc =~ obvConsHiv && length hivs > 0 = obvCa (dc, last hivs)
    | tuc =~ pneum && length pcas > 0 = obvCa (dc, last pcas)
    | otherwise = (dc, tuc)
    where
        p1 = part1 dc 
        p2 = part2 dc 
        p12 = (snd $ break (== tuc) $ (concat p1) ++ p2)
        hivs = filter (=~ hiv) p12
        pcas = filter (=~ pneumObvCa) p12

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
