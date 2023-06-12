{-# LANGUAGE DeriveGeneric #-}
module Ucrules where

import Caexprs
import Data.Csv
import Data.List
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
    | tuc == ht && length hhds > 0 = modCa (dc, last hhds)
    | tuc == ht && length hkds > 0 = modCa (dc, last hkds)
    | tuc == ht && length hhkds > 0 = modCa (dc, last hhkds)
    | tuc == ht && length hfs > 0 = modCa (dc, hhdhf)
    | tuc == ht && length hdills > 0 = modCa (dc, hhdoth)
    | tuc == ht && length strs > 0 = modCa (dc, last strs)
    | tuc == ht && length nephrs > 0 = modCa (dc, last nephrs)
    | tuc == ht && length renfs > 0 = modCa (dc, hkdrf)
    | tuc == ht && length kidcontrs > 0 = modCa (dc, hkdoth)
    | tuc == hhdhf && length rfalls > 0 = modCa (dc, hhkdhfrf)
    | tuc == hhdhf && length hkdothkdills > 0 = modCa (dc, hhkdhfoth)
    | tuc == hhdoth && length rfalls > 0 = modCa (dc, hhkdrfoth)
    | tuc == hhdoth && length hkdothkdills > 0 = modCa (dc, hhkdoth)
    | tuc == hkdrf && length hfalls > 0 = modCa (dc, hhkdhfrf)
    | tuc == hkdrf && length hhdothhdills > 0 = modCa (dc, hhkdrfoth)
    | tuc == hkdoth && length hfalls > 0 = modCa (dc, hhkdhfoth)
    | tuc == hkdoth && length hhdothhdills > 0 = modCa (dc, hhkdoth)
    | tuc =~ htd && length ihds > 0 = modCa (dc, last ihds)
    | tuc =~ nonmiihd && length mis > 0 = modCa (dc, last mis)
    | tuc == oldmi = modCa (dc, chrihdoth)
    | tuc == pulmhdnos && length scols > 0 = modCa (dc, scolhd)
    | tuc =~ hdfuncill && length ihds > 0 = modCa (dc, last ihds)
    | tuc =~ inj && length exts > 0 = modCa (dc, last exts)
    | otherwise = tuc
    where
        p1 = part1 dc 
        p2 = part2 dc 
        p12 = (concat p1) ++ p2
        hhds = filter (=~ hhd) p12
        hkds = filter (=~ hkd) p12
        hhkds = filter (=~ hhkd) p12
        ihds = filter (=~ ihd) p12
        mis = filter (=~ mi) p12
        hfs = filter (=~ hf) p12
        hdills = filter (=~ hdilldef) p12
        strs = filter (=~ stroke) p12
        nephrs = filter (=~ nephr) p12
        renfs = filter (=~ renf) p12
        kidcontrs = filter (== kidcontr) p12
        hkdrfalls = filter (=~ hkdrfall) p12
        hkdothalls = filter (=~ hkdothall) p12
        hkdothkdills = filter (=~ hkdothkdill) p12
        hhdhfalls = filter (=~ hhdhfall) p12
        hhdothalls = filter (=~ hhdothall) p12
        hhdothhdills = filter (=~ hhdothhdill) p12
        hfalls = filter (=~ hfall) p12
        hdfuncills = filter (=~ hdfuncill) p12
        rfalls = filter (=~ rfall) p12
        scols = filter (=~ scol) p12
        exts = filter (=~ ext) p12

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
        esw = words es
        p1con = filter (\c -> head c /= '6') esw
        p1 = (fmap $ drop 2) <$> (groupBy (\a b -> head a == head b) p1con)
        p2 = drop 2 <$> filter (\c -> head c == '6') esw
