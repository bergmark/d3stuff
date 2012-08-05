module Main where

import Prelude hiding (Char)
import qualified Prelude as P (Char)

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe

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
  "Str" `pint` charStr myMonk
  "Dex" `pint` charDex myMonk
  "Int" `pint` charInt myMonk
  "Vit" `pint` charVit myMonk
  dashes
  "Armor" `pint` charArmor myMonk
  "Damage" `pint` charDps myMonk
  dashes
  putStrLn "Offense"
  "Damage Increased by Primary" `perc` charPrimaryDmgBonus myMonk
  "Attacks per second" `p2` charAPS myMonk
  "Crit Chance" `perc` charCritChance myMonk
  "Crit Damage" `percint` charCritDmg myMonk
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
  "Crowd Control Reduction" `percint` charCrowdControlRed myMonk
  "Missile Damage Reduction" `percint` charMissileDmgRed myMonk
  "Melee Damage Reduction" `percint` charMeleeDmgRed myMonk
  "Thorns" `pint` charThorns myMonk
  dashes
  putStrLn "Life"
  "Maximum Life" `p` charMaxLife myMonk
  "Life Bonus" `percint` charLifeBonus myMonk
  "Life per Second" `pint` charLifeRegen myMonk
  "Life per Kill" `pint` charLifePerKill myMonk
  "Life per Hit" `pint` charLifeOnHit myMonk
  "Health Globe Healing Bonus" `pint` charHealthGlobeHeal myMonk
  "Pickup Radius" `pint` charPickupRadius myMonk
  dashes
  putStrLn "Resource"
  forM_ (charResources myMonk)
    (\r -> do
      ("Maximum " ++ show r) `pint` charResourceMax r myMonk
      (show r ++ " Regenerated per Second") `p` charResourceRegen r myMonk)
  dashes
  putStrLn "Adventure"
  "Movement Speed" `percint` charMovement myMonk
  "Gold Find" `percint` charGF myMonk
  "Magic Find" `percint` charMF myMonk
  "Bonus Exp" `percint` charBonusExp myMonk
  "Bonus Exp/Kill" `pint` charBonusExpPerKill myMonk
  dashes
  putStrLn "EHP"
  "Armor DR" `perc` charDmgRed myMonk
  "Resistance DR" `perc` charResRed myMonk
  "Total DR" `perc` charTotalRed myMonk
  "EHP" `pint` charEHP myMonk
  "Life regen" `perc` charLifeRegenPerc myMonk
  "Life on hit" `perc` charLifeOnHitPerc myMonk
  "Total Life/s" `pint` charTotalLifePerSec myMonk
  "Total life gain (regen + LoH)" `perc` charTotalLifePerSecPerc myMonk
  "Block / max HP" `perc` charBlockDmgPerc myMonk
  where
    pr :: Show s => s -> IO ()
    pr = putStr . show
    p :: Show s => String -> s -> IO ()
    p s x = putStrLn $ s ++ ": " ++ show x
    pint s x = p s (rnd x)
    p2 :: String -> Double -> IO ()
    p2 s x = putStrLn $ s ++ ": " ++ roundTwo x
    ln :: IO ()
    ln = putStrLn ""
    dashes = putStrLn "----------------"
    space = putStr " "
    perc :: String -> Double -> IO ()
    perc s x = putStrLn $ s ++ ": " ++ roundTwo (x * 100) ++ "%"
    percint s x = putStrLn $ s ++ ": " ++ show (rnd (x * 100)) ++ "%"
    rnd :: Double -> Integer
    rnd = round
    roundTwo :: Double -> String
    roundTwo = split . show
      where
        split :: String -> String
        split s = uncurry (\a b -> a ++ "." ++ b) . second (take 2 . drop 1) . splitAt (fromJust (elemIndex '.' s)) $ s

