module Items where

import Prelude hiding (Char)

import Data

keenHeavyAxeOfTheSentry :: Item
keenHeavyAxeOfTheSentry = heavyAxe {
    minDmg = 11
  , maxDmg = 21
  , elMinDmg = 1
  , elMaxDmg = 3
  , int = 23
  , ias = 0.07
  }

mendingRingOfWounding :: Item
mendingRingOfWounding = (emptyItem Ring) {
    minDmg = 2
  , maxDmg = 4
  }

keenRingOfWounding :: Item
keenRingOfWounding = (emptyItem Ring) {
    minDmg = 2
  , maxDmg = 4
  , ias = 0.02
  }

-- | Monk Items

battleChamber :: Item
battleChamber = (emptyItem Weapon1H) {
    minDmg = 363 - 256
  , maxDmg = 802 - 554
  , baseAS = 1.4
  , elMinDmg = 256
  , elMaxDmg = 554
  , dex = 126
  , int = 76
  , vit = 40
  , lifeOnHit = 737
  , critDmg = 0.61 + 0.7 -- TODO Socket
  }

forebodeingEagleOrb :: Item
forebodeingEagleOrb = (emptyItem OffHand) {
    minDmg = 1
  , maxDmg = 3
  , critArcanePower = 2
  }

keenAmulet :: Item
keenAmulet = (emptyItem Amulet) {
    ias = 0.05
  }

thiefsThrasher :: Item
thiefsThrasher = (emptyItem Gloves) {
    baseArmor = 300
  , str = 45
  , dex = 117
  , vit = 65
  , mf = 11
  , critChance = 0.05
  }

assassinsVow :: Item
assassinsVow = (emptyItem Ring) {
    minDmg = 12
  , dex = 44+46 -- TODO socket
  , vit = 15
  , ias = 0.06
  , critChance = 0.03
  }

glamourMutiny :: Item
glamourMutiny = (emptyItem Ring) {
    dex = 70
  , int = 81
  , allRes = 57
  , critChance = 0.035
  }

blackthornesBoots :: Item
blackthornesBoots = (emptyItem Boots) {
    baseArmor = 538-204
  , dex = 96
  , poisonRes = 56
  , bonusArmor = 204
  , lifeRegen = 150
  , movement = 0.012
  , eliteDamageReduction = 0.03
  }

battleGlamour :: Item
battleGlamour = (emptyItem Pants) {
    baseArmor = 632-249
  , dex = 81
  , vit = 68
  , poisonRes = 49
  , allRes = 50
  , bonusArmor = 249
  , lifeRegen = 132
  }

dreadInnocence :: Item
dreadInnocence = (emptyItem Belt) {
    baseArmor = 288
  , dex = 61
  , vit = 152
  , poisonRes = 56
  , allRes = 42
  , healthGlobeHeal = 3318
  , reducedRendCost = 4
  }

tenaciosMedallion :: Item
tenaciosMedallion = (emptyItem Amulet) {
    minDmg = 10
  , maxDmg = 21
  , str = 108
  , dex = 120
  , coldRes = 37
  , bonusArmor = 339
  , critDmg = 0.49
  }

banishedVestments :: Item
banishedVestments = (emptyItem Chest) {
    baseArmor = 360
  , dex = 85 + 46
  , int = 59
  , vit = 67
  , poisonRes = 47
  , allRes = 58
  , thorns = 122
  }

hellStyle :: Item
hellStyle = (emptyItem Shoulders) {
    baseArmor = 293
  , dex = 113
  , vit = 48
  , poisonRes = 42
  , allRes = 53
  , lifeBonus = 0.07
  }

andarielsVisage :: Item
andarielsVisage = (emptyItem Helm) {
    baseArmor = 274
  , dex = 113
  , poisonRes = 33
  , ias = 0.06
  , lifeRegen = 87
  , critChance = 0.03
  , lifeBonus = 0.15
  }

forgeSerpent :: Item
forgeSerpent = (emptyItem Weapon1H) {
    baseAS = 1.2
  , minDmg = 344 - 155
  , maxDmg = 694 - 345
  , elMinDmg = 155
  , elMaxDmg = 345
  , str = 199
  , dex = 261
  , critDmg = 0.66 + 0.70 -- TODO socket
  }

shackleCore :: Item
shackleCore = (emptyItem Wrists) {
    baseArmor = 203
  , dex = 89
  , int = 84
  , vit = 85
  , allRes = 51
  , critChance = 0.045
  }

ruggedWall :: Item
ruggedWall = (emptyItem Shield) {
    baseArmor = 1102
  , baseBlockChance = 0.14
  , blockAmountMin = 3706
  , blockAmountMax = 4706
  , bonusBlockChance = 0
  , dex = 93 + 42 -- TODO socket
  , vit = 92
  , physicalRes = 29
  , lifeBonus = 0.05
  , critChance = 0.1
  , corpseSpiderDmg = 0.12
  }

exampleWiz :: Char
exampleWiz = (emptyChar Wizard 10) {
    weapon1 = keenHeavyAxeOfTheSentry
  , offHand = forebodeingEagleOrb
  , ring1 = mendingRingOfWounding
  , ring2 = keenRingOfWounding
  , amulet = keenAmulet
  }

myMonk :: Char
myMonk = (emptyChar Monk 60) {
     weapon1 = battleChamber
--     weapon1 = forgeSerpent
   , offHand = forgeSerpent
--   , offHand = ruggedWall

   , amulet = tenaciosMedallion
   , belt = dreadInnocence
   , boots = blackthornesBoots
   , chest = banishedVestments
   , gloves = thiefsThrasher
   , helm = andarielsVisage
   , pants = battleGlamour
   , ring1 = assassinsVow
   , ring2 = glamourMutiny
   , shoulders = hellStyle
   , wrists = shackleCore

   , chantOfResonance = True
   , exaltedSoul = False
   , oneWithEverything = True
   , seizeTheInitiative = True
  }

