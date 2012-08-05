module Data where

import Prelude hiding (Char)

avg :: Double -> Double -> Double
avg a b = (a+b)/2

if0 :: Num a => Bool -> a -> a
if0 b a = if b then a else 0

data Slot =
    Amulet
  | Belt
  | Boots
  | Chest
  | Gloves
  | Helm
  | OffHand
  | Pants
  | Ring
  | Shield
  | Shoulders
  | Shrine
  | Weapon1H
  | Wrists
    deriving (Show, Eq)

data Buff =
  -- Monk Passives
    ChantOfResonance
  | ExaltedSoul
  | OneWithEverything
  | SeizeTheInitiative
    deriving (Eq, Show)

charHasBuff :: Buff -> Char -> Bool
charHasBuff b c = b `elem` buffs c

data Item = Item {
    slot :: Slot
  , allRes :: Double
  , apsBonus :: Double
  , arcaneRes :: Double
  , baseArmor :: Double
  , baseAS :: Double
  , baseBlockChance :: Double
  , blockAmountMin :: Double
  , blockAmountMax :: Double
  , bonusArmor :: Double
  , bonusBlockChance :: Double
  , bonusExp :: Double
  , bonusExpPerKill :: Double
  , coldRes :: Double
  , corpseSpiderDmg :: Double
  , critArcanePower :: Double
  , critChance :: Double
  , critDmg :: Double
  , crowdControlRed :: Double
  , dex :: Double
  , eliteDamageReduction :: Double
  , elMaxDmg :: Double
  , elMinDmg :: Double
  , fireRes :: Double
  , gf :: Double
  , healthGlobeHeal :: Double
  , ias :: Double
  , int :: Double
  , lifeBonus :: Double
  , lifeOnHit :: Double
  , lifePerKill :: Double
  , lifeRegen :: Double
  , lightningRes :: Double
  , maxDmg :: Double
  , meleeDmgRed :: Double
  , mf :: Double
  , minDmg :: Double
  , missileDmgRed :: Double
  , movement :: Double
  , physicalRes :: Double
  , pickupRadius :: Double
  , poisonRes :: Double
  , reducedRendCost :: Double
  , spiritRegen :: Double
  , str :: Double
  , thorns :: Double
  , vit :: Double
  } deriving (Show, Eq)

armor :: Item -> Double
armor i = baseArmor i + bonusArmor i

emptyItem :: Slot -> Item
emptyItem s = Item {
    slot = s
  , allRes = 0
  , apsBonus = 0 -- +0.13 Attacks Per Second
  , arcaneRes = 0
  , baseAS = 0
  , baseArmor = 0
  , baseBlockChance = 0
  , blockAmountMax = 0
  , blockAmountMin = 0
  , bonusArmor = 0
  , bonusBlockChance = 0
  , bonusExp = 0
  , bonusExpPerKill = 0
  , coldRes = 0
  , corpseSpiderDmg = 0
  , critArcanePower = 0
  , critChance  = 0
  , critDmg = 0
  , crowdControlRed = 0
  , dex = 0
  , elMaxDmg = 0
  , elMinDmg = 0
  , eliteDamageReduction = 0
  , fireRes = 0
  , gf = 0
  , healthGlobeHeal = 0
  , ias = 0 -- Increases attack speed by 7%
  , int = 0
  , lifeBonus = 0
  , lifeOnHit = 0
  , lifePerKill = 0
  , lifeRegen = 0
  , lightningRes = 0
  , maxDmg = 0
  , meleeDmgRed = 0
  , mf = 0
  , minDmg = 0
  , missileDmgRed = 0
  , movement = 0
  , physicalRes = 0
  , pickupRadius = 0
  , poisonRes = 0
  , reducedRendCost = 0
  , spiritRegen = 0
  , str = 0
  , thorns = 0
  , vit = 0
  }

heavyAxe :: Item
heavyAxe = (emptyItem Weapon1H) {
    baseAS = 1.3
  }

frenzyShrine :: Item
frenzyShrine = (emptyItem Shrine) {
    ias = 0.25
  }

data Klass = Barbarian | DemonHunter | Monk | WitchDoctor | Wizard
           deriving (Eq, Show)

data Char = Char {
    level :: Double
  , klass :: Klass

  , amulet :: Item
  , belt :: Item
  , boots :: Item
  , chest :: Item
  , gloves :: Item
  , helm :: Item
  , offHand :: Item
  , pants :: Item
  , ring1 :: Item
  , ring2 :: Item
  , shoulders :: Item
  , weapon1 :: Item
  , wrists :: Item

  -- Shrine bonuses
  , passiveBonus :: Item

  , buffs :: [Buff]
  }

isBarbarian :: Char -> Bool
isBarbarian = (== Barbarian) . klass

isDemonHunter :: Char -> Bool
isDemonHunter = (== DemonHunter) . klass

isMonk :: Char -> Bool
isMonk = (== Monk) . klass

isWitchDoctor :: Char -> Bool
isWitchDoctor = (== WitchDoctor) . klass

isWizard :: Char -> Bool
isWizard = (== Wizard) . klass

itemList :: Char -> [Item]
itemList c = map ($ c) [
    amulet
  , belt
  , boots
  , chest
  , gloves
  , helm
  , offHand
  , pants
  , ring1
  , ring2
  , shoulders
  , weapon1
  , wrists]

emptyChar :: Klass -> Double -> Char
emptyChar kls lvl = Char {
    klass = kls
  , level = lvl

  , amulet = emptyItem Amulet
  , belt = emptyItem Belt
  , boots = emptyItem Boots
  , chest = emptyItem Chest
  , gloves = emptyItem Gloves
  , helm = emptyItem Helm
  , offHand = emptyItem OffHand
  , pants = emptyItem Pants
  , ring1 = emptyItem Ring
  , ring2 = emptyItem Ring
  , shoulders = emptyItem Shoulders
  , weapon1 = emptyItem Weapon1H
  , wrists = emptyItem Wrists

  , passiveBonus = frenzyShrine

  , buffs = []
  }

-- | Calculations

basePrimaryAttrValue :: Char -> Double
basePrimaryAttrValue char = 10 + 3*(level char - 1)

baseSecondaryAttrValue :: Char -> Double
baseSecondaryAttrValue char = 7 + level char

charBaseStr :: Char -> Double
charBaseStr char
  | isBarbarian char = basePrimaryAttrValue char
  | otherwise = baseSecondaryAttrValue char

charBaseDex :: Char -> Double
charBaseDex char
  | isMonk char || isDemonHunter char = basePrimaryAttrValue char
  | otherwise = baseSecondaryAttrValue char

charBaseInt :: Char -> Double
charBaseInt char
  | isWizard char || isWitchDoctor char = basePrimaryAttrValue char
  | otherwise = baseSecondaryAttrValue char

charBaseVit :: Char -> Double
charBaseVit char = 9 + 2*(level char - 1)

charStr :: Char -> Double
charStr char = charBaseStr char + (sum . map str . itemList $ char)

charDex :: Char -> Double
charDex char = charBaseDex char + (sum . map dex . itemList $ char)

charInt :: Char -> Double
charInt char = charBaseInt char + (sum . map int . itemList $ char)

charVit :: Char -> Double
charVit char = charBaseVit char + (sum . map vit . itemList $ char)

primaryAttrValue :: Char -> Double
primaryAttrValue c = (case klass c of
  Barbarian -> charStr
  DemonHunter -> charDex
  Monk -> charDex
  WitchDoctor -> charInt
  Wizard -> charInt) c

charArmor :: Char -> Double
charArmor c = charStr c + sumField armor c + if0 (charHasBuff SeizeTheInitiative c) (charDex c)

sumField :: (Item -> Double) -> Char -> Double
sumField f = sum . map f . itemList

charPrimaryDmgBonus :: Char -> Double
charPrimaryDmgBonus c = primaryAttrValue c / 100

charSkillDmgBonus :: Char -> Double
charSkillDmgBonus = const 0

baseCritChance :: Double
baseCritChance = 0.05

charCritChance :: Char -> Double
charCritChance c = baseCritChance + sumField critChance c

baseCritDmg :: Double
baseCritDmg = 0.5

charCritDmg :: Char -> Double
charCritDmg c = baseCritDmg + sumField critDmg c

charAPS :: Char -> Double
charAPS c
  -- TODO fix (too high with 1.15, too low without)
  | charDualWields c = 1.15 * (waps w1 `avg` waps w2)
  | otherwise = waps w1
  where
    waps w = (charWeaponSpeed w c + apsBonus w) * nonWeaponIAS c
    w1 = weapon1 c
    w2 = offHand c

damage :: Item -> Char -> Double
damage wpn c = minDmg wpn + maxDmg wpn + elMinDmg wpn + elMaxDmg wpn + bonusDmg
  where
    -- +min/max dmg from non-weapons
    bonusDmg :: Double
    bonusDmg = sum . map (\i -> minDmg i + maxDmg i) . filter itemFilter $ itemList c
    itemFilter :: Item -> Bool
    itemFilter = if charDualWields c
      then not . (`elem` [weapon1 c, offHand c])
      else (/= wpn)

weaponDps :: Item -> Double
weaponDps w = (minDmg w `avg` maxDmg w) * weaponSpeed' w

weaponSpeed' :: Item -> Double
weaponSpeed' w = baseAS w * (1 + ias w) + apsBonus w

-- Weapon speed with dual wield factored in
charWeaponSpeed :: Item -> Char -> Double
charWeaponSpeed w c
  | charDualWields c = 2 / (1 / (baseAS w1 * (1 + ias w1) + apsBonus w1) + 1 / (baseAS w2 * (1 + ias w2) + apsBonus w2))
  | otherwise = baseAS w * (1 + ias w) + apsBonus w
  where
    w1 = weapon1 c
    w2 = offHand c
-- TODO factor in aps for a second weapon?

nonWeaponIAS :: Char -> Double
nonWeaponIAS c
  | charDualWields c = 1 + sumField ias c - ias (weapon1 c) - ias (offHand c) + 0.15
  | otherwise = 1 + sumField ias c - ias (weapon1 c)

charDps :: Char -> Double
charDps c
    | charDualWields c = charWeaponDps (weapon1 c) c `avg` charWeaponDps (offHand c) c
    | otherwise = charWeaponDps (weapon1 c) c
  where

charWeaponDps :: Item -> Char -> Double
charWeaponDps wpn c = damage wpn c * charWeaponSpeed wpn c * nonWeaponIAS c * (1 + charCritChance c * charCritDmg c) * (1 + charPrimaryDmgBonus c) / 2

charDualWields :: Char -> Bool
charDualWields c = slot (weapon1 c) == Weapon1H && slot (offHand c) == Weapon1H

charBlockAmountMin :: Char -> Double
charBlockAmountMin = blockAmountMin . offHand

charBlockAmountMax :: Char -> Double
charBlockAmountMax = blockAmountMax . offHand

charBlockChance :: Char -> Double
charBlockChance c = sumField baseBlockChance c + sumField bonusBlockChance c

charDodgeChance :: Char -> Double
charDodgeChance c
    | dext < 101 = (0.1*dext)/100
    | dext < 501 = (10+0.025*(dext-100))/100
    | dext < 1001 = (20 + 0.02*(dext-500))/100
    | otherwise = (30 + 0.01*(dext-1000))/100
  where
    dext = charDex c

-- All res with passive bonuses
charAllRes :: Char -> Double
charAllRes c
    | charHasBuff OneWithEverything c = oweMaxRes c
    | otherwise = _charAllRes c

-- Only the allres stat and int
_charAllRes :: Char -> Double
_charAllRes c = sumField allRes c + charInt c/10

oweMaxRes :: Char -> Double
oweMaxRes c = _charAllRes c + maximum [
    _charPhysicalRes c
  , _charColdRes c
  , _charFireRes c
  , _charLightningRes c
  , _charPoisonRes c
  , _charArcaneRes c
  ]

-- Resistances without OWE and allres
_charPhysicalRes :: Char -> Double
_charPhysicalRes = sumField physicalRes
_charColdRes :: Char -> Double
_charColdRes = sumField coldRes
_charFireRes :: Char -> Double
_charFireRes = sumField fireRes
_charLightningRes :: Char -> Double
_charLightningRes = sumField lightningRes
_charPoisonRes :: Char -> Double
_charPoisonRes = sumField poisonRes
_charArcaneRes :: Char -> Double
_charArcaneRes = sumField arcaneRes

charPhysicalRes :: Char -> Double
charPhysicalRes c
  | charHasBuff OneWithEverything c = oweMaxRes c
  | otherwise = _charAllRes c + sumField physicalRes c
charColdRes :: Char -> Double
charColdRes c
  | charHasBuff OneWithEverything c = oweMaxRes c
  | otherwise = _charAllRes c + sumField coldRes c
charFireRes :: Char -> Double
charFireRes c
  | charHasBuff OneWithEverything c = oweMaxRes c
  | otherwise = _charAllRes c + sumField fireRes c
charLightningRes :: Char -> Double
charLightningRes c
  | charHasBuff OneWithEverything c = oweMaxRes c
  | otherwise = _charAllRes c + sumField lightningRes c
charPoisonRes :: Char -> Double
charPoisonRes c
  | charHasBuff OneWithEverything c = oweMaxRes c
  | otherwise = _charAllRes c + sumField poisonRes c
charArcaneRes :: Char -> Double
charArcaneRes c
  | charHasBuff OneWithEverything c = oweMaxRes c
  | otherwise = _charAllRes c + sumField arcaneRes c

charCrowdControlRed :: Char -> Double
charCrowdControlRed = sumField crowdControlRed

charMissileDmgRed :: Char -> Double
charMissileDmgRed = sumField missileDmgRed

charMeleeDmgRed :: Char -> Double
charMeleeDmgRed = sumField meleeDmgRed

charThorns :: Char -> Double
charThorns = sumField thorns

charMovement :: Char -> Double
charMovement c = 0.25 `max` sumField movement c

charGF :: Char -> Double
charGF = sumField gf

charMF :: Char -> Double
charMF = sumField mf

charBonusExp :: Char -> Double
charBonusExp = sumField bonusExp

charBonusExpPerKill :: Char -> Double
charBonusExpPerKill = sumField bonusExpPerKill

data Resource =
    Fury
  | Hatred
  | Discipline
  | Spirit
  | Mana
  | ArcanePower
  deriving (Eq, Show)

charResources :: Char -> [Resource]
charResources c = case klass c of
    Barbarian -> [Fury]
    DemonHunter -> [Hatred, Discipline]
    Monk -> [Spirit]
    WitchDoctor -> [Mana]
    Wizard -> [ArcanePower]

monkSpiritMax :: Char -> Double
monkSpiritMax c
    | charHasBuff ExaltedSoul c = 100 + 150
    | otherwise = 150

-- TODO Numbers for all classes
charResourceMax :: Resource -> Char -> Double
charResourceMax Fury        c | isBarbarian c   = -1
charResourceMax Hatred      c | isDemonHunter c = -1
charResourceMax Discipline  c | isDemonHunter c = -1
charResourceMax Spirit      c | isMonk c        = monkSpiritMax c
charResourceMax Mana        c | isWitchDoctor c = -1
charResourceMax ArcanePower c | isWizard c      = -1
charResourceMax r c = error $ concat [
  show (klass c), " does not have the resource ", show r]

-- TODO Numbers for all classes
charResourceRegen :: Resource -> Char -> Double
charResourceRegen Fury c | isBarbarian c = -1
charResourceRegen Hatred c | isDemonHunter c = -1
charResourceRegen Discipline c | isDemonHunter c = -1
charResourceRegen Spirit c | isMonk c = sumField spiritRegen c + if0 (charHasBuff ChantOfResonance c) 2
charResourceRegen Mana c | isWitchDoctor c = -1
charResourceRegen ArcanePower c | isWizard c = -1
charResourceRegen r c = error $ concat [
  show (klass c), " does not have the resource ", show r]

charMaxLife :: Char -> Double
charMaxLife c
  | level c >= 35 = (36 + 4*lvl + (lvl-25)*vita)*(1 + life)
  | otherwise = (36 + 4*lvl + 10*vita)*(1 + life)
    where
      lvl = level c
      vita = charVit c
      life = charLifeBonus c

charLifeBonus :: Char -> Double
charLifeBonus = sumField lifeBonus

charLifeRegen :: Char -> Double
charLifeRegen = sumField lifeRegen

charLifeOnHit :: Char -> Double
charLifeOnHit = sumField lifeOnHit

charHealthGlobeHeal :: Char -> Double
charHealthGlobeHeal = sumField healthGlobeHeal

charPickupRadius :: Char -> Double
charPickupRadius = sumField pickupRadius

charLifePerKill :: Char -> Double
charLifePerKill = sumField lifePerKill

-- | Survivability (EHP, reductions, regen%)

charDmgRed :: Char -> Double
charDmgRed c = arm / (50 * level c + arm)
  where
    arm = charArmor c

charResRed :: Char -> Double
charResRed c = aRes / (5 * level c + aRes)
  where
    aRes = charAllRes c

charInnateDR :: Char -> Double
charInnateDR c = case klass c of
  Barbarian -> 0.3
  DemonHunter -> 0
  Monk -> 0.3
  WitchDoctor -> 0 -- TODO add passive
  Wizard -> 0

charEHP :: Char -> Double
charEHP c = charMaxLife c / ((1-charDmgRed c) * (1-charResRed c) * (1-charInnateDR c))

charTotalRed :: Char -> Double
charTotalRed c = 1 - (1 - charDmgRed c)*(1 - charResRed c)

charLifeRegenPerc :: Char -> Double
charLifeRegenPerc c = charLifeRegen c / charMaxLife c

charLifeOnHitPerc :: Char -> Double
charLifeOnHitPerc c = charLifeOnHit c / charMaxLife c

charTotalLifePerSec :: Char -> Double
charTotalLifePerSec c = charLifeRegen c + (charAPS c * charLifeOnHit c)

charTotalLifePerSecPerc :: Char -> Double
charTotalLifePerSecPerc c = charTotalLifePerSec c / charMaxLife c

charBlockAmountAvg :: Char -> Double
charBlockAmountAvg c = charBlockAmountMax c `avg` charBlockAmountMin c

charBlockDmgPerc :: Char -> Double
charBlockDmgPerc c = charBlockAmountAvg c / charMaxLife c
