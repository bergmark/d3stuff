module Main where

import Prelude hiding (Char)

import Control.Monad

import Data
import Items

main :: IO ()
main = do
--  print battleChamber
--  print $ weaponSpeed battleChamber
--  print $ weaponDps battleChamber
--  print $ weaponSpeed keenHeavyAxeOfTheSentry
--  print $ weaponDps keenHeavyAxeOfTheSentry
--  print $ damage exampleWiz
--  print $ nonWeaponASBonus exampleWiz
--  print $ critical exampleWiz
--  print $ primary exampleWiz
--  print $ charDps exampleWiz
  dashes

  putStr "Base SDIV: "
  pr $ charBaseStr myMonk
  space
  pr $ charBaseDex myMonk
  space
  pr $ charBaseInt myMonk
  space
  pr $ charBaseVit myMonk
  ln
  dashes

  putStrLn "Attributes: "
  "Str" `p` charStr myMonk
  "Dex" `p` charDex myMonk
  "Int" `p` charInt myMonk
  "Vit" `p` charVit myMonk
  dashes
  "Armor" `p` charArmor myMonk
  "Damage" `p` charDps myMonk
  dashes
  putStrLn "Offense"
  "Damage Increased by Primary" `perc` charPrimaryDmgBonus myMonk
  "Attacks per second" `p` charAPS myMonk
  "Crit Chance" `perc` charCritChance myMonk
  "Crit Damage" `perc` charCritDmg myMonk
  dashes
  putStrLn "Defense"
  "Block Amount" `p` (charBlockAmountMin myMonk, charBlockAmountMax myMonk)
  "Block Chance" `perc` charBlockChance myMonk
  "Dodge Chance" `perc` charDodgeChance myMonk
  "Damage Reduction" `perc` charDmgRed myMonk
  "Physical Res" `p` charPhysicalRes myMonk
  "Cold Res" `p` charColdRes myMonk
  "Fire Res" `p` charFireRes myMonk
  "Lightning Res" `p` charLightningRes myMonk
  "Poison Res" `p` charPoisonRes myMonk
  "Arcane Res" `p` charArcaneRes myMonk
  "Crowd Control Reduction" `p` charCrowdControlRed myMonk
  "Missile Damage Reduction" `p` charMissileDmgRed myMonk
  "Melee Damage Reduction" `p` charMeleeDmgRed myMonk
  "Thorns" `p` charThorns myMonk
  dashes
  putStrLn "Life"
  "Maximum Life" `p` charMaxLife myMonk
  "Life Bonus" `perc` charLifeBonus myMonk
  "Life per Second" `p` charLifeRegen myMonk
  "Life per Kill" `p` charLifePerKill myMonk
  "Life per Hit" `p` charLifeOnHit myMonk
  "Health Globe Healing Bonus" `p` charHealthGlobeHeal myMonk
  "Pickup Radius" `p` charPickupRadius myMonk
  dashes
  putStrLn "Resource"
  forM_ (charResources myMonk)
    (\r -> do
      ("Maximum " ++ show r) `p` charResourceMax r myMonk
      (show r ++ "Regenerated per Second") `p` charResourceRegen r myMonk)
  dashes
  putStrLn "Adventure"
  "Movement Speed" `perc` charMovement myMonk
  "Gold Find" `perc` charGF myMonk
  "Magic Find" `perc` charMF myMonk
  "Bonus Exp" `perc` charBonusExp myMonk
  "Bonus Exp/Kill" `p` charBonusExpPerKill myMonk
  where
    pr :: Show s => s -> IO ()
    pr = putStr . show
    p :: Show s => String -> s -> IO ()
    p s x = putStrLn $ s ++ ": " ++ show x
    ln :: IO ()
    ln = putStrLn ""
    dashes = putStrLn "----------------"
    space = putStr " "
    perc :: String -> Double -> IO ()
    perc s x = putStrLn $ s ++ ": " ++ (show (x * 100)) ++ "%"
