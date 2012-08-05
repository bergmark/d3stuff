module Data where

import Prelude hiding (Char)

avg :: Double -> Double -> Double
avg a b = (a+b)/2

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

  , weapon1 :: Item
  , offHand :: Item
  , ring1 :: Item
  , ring2 :: Item
  , amulet :: Item
  , gloves :: Item
  , boots :: Item
  , pants :: Item
  , chest :: Item
  , belt :: Item
  , shoulders :: Item
  , wrists :: Item
  , helm :: Item

  -- Shrine bonuses
  , passiveBonus :: Item

  -- Monk passives
  , exaltedSoul :: Bool
  , oneWithEverything :: Bool
  , seizeTheInitiative :: Bool
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

  , exaltedSoul = False
  , oneWithEverything = False
  , seizeTheInitiative = False
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
charArmor c = charStr c + itemArmor c + stiBonus c
  where
    itemArmor = sumField armor
    stiBonus c' = if seizeTheInitiative c' then charDex c' else 0

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
charAPS c = (weaponSpeed wpn + apsBonus wpn) * nonWeaponIAS c
  where
    wpn = weapon1 c

damage :: Char -> Double
damage char = minDmg wpn + maxDmg wpn + elMinDmg wpn + elMaxDmg wpn + bonusDmg
  where
    wpn = weapon1 char
    bonusDmg = sum . map (\i -> minDmg i + maxDmg i) . filter (/= wpn) . itemList $ char

weaponDps :: Item -> Double
weaponDps w = (minDmg w `avg` maxDmg w) * weaponSpeed w

weaponSpeed :: Item -> Double
weaponSpeed w = baseAS w * (1 + ias w) + apsBonus w
-- TODO factor in aps for a second weapon?

nonWeaponIAS :: Char -> Double
nonWeaponIAS c =
  1 + sumField ias c - ias wpn
  where
    wpn = weapon1 c

charDps :: Char -> Double
charDps c = damage c * weaponSpeed (weapon1 c) * nonWeaponIAS c * (1 + charCritChance c * charCritDmg c) * (1 + charPrimaryDmgBonus c) / 2

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

charDmgRed :: Char -> Double
charDmgRed c = arm / (50 * level c + arm)
  where
    arm = charArmor c

charAllRes :: Char -> Double
charAllRes c = sumField allRes c + charInt c/10

oweMaxRes :: Char -> Double
oweMaxRes c = charAllRes c + maximum [
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
  | oneWithEverything c = oweMaxRes c
  | otherwise = charAllRes c + sumField physicalRes c
charColdRes :: Char -> Double
charColdRes c
  | oneWithEverything c = oweMaxRes c
  | otherwise = charAllRes c + sumField coldRes c
charFireRes :: Char -> Double
charFireRes c
  | oneWithEverything c = oweMaxRes c
  | otherwise = charAllRes c + sumField fireRes c
charLightningRes :: Char -> Double
charLightningRes c
  | oneWithEverything c = oweMaxRes c
  | otherwise = charAllRes c + sumField lightningRes c
charPoisonRes :: Char -> Double
charPoisonRes c
  | oneWithEverything c = oweMaxRes c
  | otherwise = charAllRes c + sumField poisonRes c
charArcaneRes :: Char -> Double
charArcaneRes c
  | oneWithEverything c = oweMaxRes c
  | otherwise = charAllRes c + sumField arcaneRes c

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
    | exaltedSoul c = 100 + 150
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
charResourceRegen Spirit c | isMonk c = sumField spiritRegen c
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
