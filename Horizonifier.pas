{
    Attempts to automatically tag items and adjust recipes for Horizon
    Additionally, test files can be put into Edit Scripts\vis-overrides\ in the format of
        editor_id=[VIS TAG] Name
        other_editor_id=Any Name whatsoever
    for manual renaming configuration
}
unit Horizonifier;

    //uses VisOverrides;
    //uses praFunctions;
    uses praUtil;
    uses praTagifier;


    var
        ToFile: IInterface;
        effectsToRemove: TStringList;
        horizonFile: IwbFile;
        architectFile: IwbFile;
        mainFile: IwbFile;
        exeFile: IwbFile;
        isSuccessful: boolean;
        setupDone: boolean;

        workbenchChemlabKw: IInterface; // Chemlab
        utilityCategoryKw: IInterface; // RecipeUtility
        utility2CategoryKw: IInterface; // RecipeMisc

        perkArmorer1: IInterface; // perk Armorer01
        perkBlacksmith1: IInterface; // perk Blacksmith01
        //perkScience1: IInterface; // perk Science01
        //perkGunNut1: IInterface; // perk GunNut01

        capsItem: IInterface;

        vendorContract: IInterface;
        vendorCredit: IInterface;

        // horizon benches
        WorkbenchWeaponsLab: IInterface;
        WorkbenchRoboticsLab: IInterface;

        // horizon cats
        RecipeRobotics02: IINterface; // tools and parts
        RecipeRobotics01: IINterface; // devices
        RecipeRobotics_Weapons: IInterface; // weapons;
        RecipeRobotics_AmmoSpecial: IInterface; // ammo special;
        RecipeIngredients: IInterface; // ingredients;

        BuildManager, ResourceManager, WorkshopManager: IInterface;

        // vanilla cats
        RecipeGrenade: IInterface;
        RecipeMines: IInterface;

        replaceableFnamList: TStringList;

        objectTypeKeywords: TStringList;

        // more perks
        // LocalLeader01: IInterface;
        // LocalLeader02: IInterface;
        // Medic01 : IInterface;
        // CapCollector01 : IInterface;
        // from horizon
        // Z_PerkSettlerSkillSmith01: IInterface;
        // LW
        // LocTypeWorkshopSettlement: IInterface;

        // components
        c_ExplosiveMaterial: IInterface;
        c_Fertilizer: IInterface;

        c_Leather: IInterface;
        c_Cloth: IInterface;
        c_LaserParts: IInterface;
        c_ShotgunParts: IInterface;
        c_PistolParts: IInterface;
        c_PipeParts: IInterface;
        c_Glass: IInterface;
        c_Antiseptic: IInterface;
        c_LiquidFuel: IInterface;
        SurgicalScissors: IInterface;
        SurgicalScalpel: IInterface;
        Scissors: IInterface;
        ToolKit: IInterface;
        ToolKitPrescision: IInterface;
        ToolKitCooking: IInterface;

        //Item_TurretKit_Arc: IInterface;

        // Horizon skills:
        skillScience03: IInterface;
        skillBallistics02: IInterface;
        skillLeadership01: IInterface;
        skillLeadership02: IInterface;
        skillTrading01: IInterface;
        skillTrading02: IInterface;
        Z_PerkSettlerTechnology02: IInterface;
        Z_PerkSettlerTechnology03: IInterface;
        Z_PerkSettlerTechnology04: IInterface;
        Z_PerkSettlerTechnology05: IInterface;

        // AVs
        avWaterProd: IInterface;
        avPowerReq: IInterface;
        avHappinessProd: IInterface;

        // item type keywords, all from architect
        ObjectTypeUtility : IInterface;// ObjectTypeUtility [KYWD:0C002C83], used by bobby pins, maintenance/tool/supply kits, scrap kits, auto lockpicks
        ObjectTypeBook : IInterface;    //ObjectTypeBook [KYWD:0C002C66], consumable notes, newspapers
        ObjectTypeBuff : IInterface;//ObjectTypeBuff [KYWD:0C002C67], good chems, rad-x, also stealth boys
        ObjectTypeCure : IInterface; //ObjectTypeCure [KYWD:0C002C69], radAway,
        ObjectTypeCurrency : IInterface; //ObjectTypeCurrency [KYWD:0C002C6A]
        //ObjectTypeChem: IInterface;// [KYWD:000F4AE7]
        ObjectTypeDevice : IInterface; // ObjectTypeDevice [KYWD:0C002C6B] devices and usable tools
        ObjectTypeFoodClean: IInterface; // ObjectTypeFoodClean [KYWD:0C002C6E], cooked food, usually rad-free
        ObjectTypeFoodPrewar: IInterface;//ObjectTypeFoodPrewar [KYWD:0C002C6F] pre-war food with rads
        ObjectTypeFoodRaw: IInterface; // ObjectTypeFoodRaw [KYWD:0C002C70] raw food
        ObjectTypeJunkNoValue: IInterface; // ObjectTypeJunkNoValue [KYWD:0C002C73] junk value = 0
        ObjectTypeJunkCommon: IInterface; //ObjectTypeJunkCommon [KYWD:0C002D28] also fiberglass, circuitry, allowys? value >= 10
        ObjectTypeJunkCommonLow : IInterface; // ObjectTypeJunkCommonLow [KYWD:0C002C71] generic junk? value < 10 maybe? glass, steel, wood, rubber, screw, packing material, plastic, ceramic, asbestos, spring, gear, aluminum, cloth
        // ObjectTypeJunkCommonMedical [KYWD:0C002C72] probably impossible
        ObjectTypeJunkOrganic: IInterface; //ObjectTypeJunkOrganic [KYWD:0C002C74], bone, leather, also acid, antiseptic, packing material, nuclear material
        ObjectTypeJunkQuality : IInterface;//ObjectTypeJunkQuality [KYWD:0C002C75], cork, plastic, screw, cryogenic fluid, rubber, steel, asbestos, gear, circuitry, acid, fiberglass, antiseptic, oil, copper, nuclear material, lead, crystal, fiber optics, aluminum, ballistic fiber, gold, silver, any alloy
        ObjectTypeMedical: IInterface; //ObjectTypeMedical [KYWD:0C002C76] stimpacks, bandages, probably unnecessary
        ObjectTypeQuest : IInterface;// [KYWD:0C002C78] probably impossible
        ObjectTypeSellable : IInterface; // ObjectTypeSellable [KYWD:0C002C79] valuable
        ObjectTypeSellableCollectable : IInterface;// [KYWD:0C002C7A] probably impossible?
        ObjectTypeShipment : IInterface; // ObjectTypeShipment [KYWD:0C002C7C] shipment
        // ObjectTypeIngredient [KYWD:0C002C7D] probably impossible
        ObjectTypeWaterClean : IInterface; // ObjectTypeWaterClean [KYWD:0C002C80] clean water, possible?
        // ObjectTypeJunkLegendary [KYWD:0C002C82] special horizon stuff, probably impossible
        // ObjectTypeDesign [KYWD:0C0010A3] useful for simsettlements, but otherwise? There are also tags (Foundation), (City), (Store), (Bunker), (Town), (House). Probably use (House) for all blueprints.
        //ObjectTypeAlcohol: IInterface;// [KYWD:0010C416]
        ObjectTypeJunkLegendary : IInterface;// [KYWD:0C002C82]


        // spells
        //WaterFountainCleanDrinking "Cool, delicious, refreshing water" [SPEL:000B1ECD]
        spellWaterClean: IInterface;
        //WaterRadiationDrinking "Cool, delicious, refreshing water" [SPEL:00024FBF]
        spellWaterRads: IInterface;


    procedure initTags();
    begin
        settingAddComponentString := true;

        tagShipment     := '[  ]';
        tagScrap        := '[Junk]';
        tagResource     := '(_)';
        tagLooseMod     := '[Mod]';
        tagCollectible  := '[Collectible]';
        tagQuest        := '[Quest]';
        tagCurrency     := '(Currency)';
        tagValuable     := '($)';
        tagOtherMisc    := '[Misc]'; // trash
        tagGoodChem     := '(+)';
        tagBadChem      := '(~)';
        tagFood         := '[Food]'; // generic food, selfcooked. Usually radless
        tagFoodRaw      := '{Raw}'; // raw food, has rads, has disease risk
        tagFoodCrop     := '{Crops}'; // crops, has rads
        tagFoodPrewar   := '{Prewar}'; // prewar packaged, has rads
        tagDrink        := '[=]';
        tagLiquor       := '{Liquor}';
        tagNukacola     := '[=C]';
        tagSyringe      := '[Syringe]';
        tagDevice       := '(^)';
        tagTool         := '(X)';
        tagNews         := '[#Note]';
        tagNote         := '[#Note]';
        tagPerkmag      := '[Mag]';
        tagMine         := '(Mine)';
        tagGrenade      := '(Grenade)';
        tagKey          := '[Key]';
        tagAmmo         := '(Ammo)';
        tagHolotape     := '[#Holotape]';
        tagHolotapeGame := '[#Holotape]';
        tagHolotapeSettings := '[Settings]';
        tagPipBoy       := '|Pip|';
        
        extraValidTags  := 'Aid,+,+T,++,Cure,-,-X,-R,Device,Buff,Schematic,!Sleep,FM,!Rest,^,^D,^A,^T,^R,^M,^Fish,^S,$,$$,X$,?,*,:,!,Can,!P,!W,!Y,!T,#W,#SK,#C,#B,#P,D-,#^,#,##,__,CM,--, ,  ,_,_#,!!,X,XX,Y,J,Junk,JunkP,Model,Food,FoodC,Meal,Crops,Crop,Chem,ChemC,Energy,~,~C,~A,Liquor,=L,=,==,=C,Nuka,NukaC,Prewar,I,Raw,Wild,Ingredients,Syringe,Ammo,XAmmo,AmmoN,AmmoM,Fuel,Core,PA-AL,PA-AR,PA-C,PA-H,PA-LL,PA-LR,PA,ArmL,ArmR,Arm,Gloves,Chest,Vault,LegL,LegR,Leg,Bandolier,Belt,Biosuit,!Hazmat,Hazmat,Bottom,Bracelet,Cloak,Dog,Earring,Footwear,Harness,Hat,#Hat,Headband,#Headband,Eyewear,#Eyewear,Helmet,#Helmet,Head,#Head,H,Jacket,Mouth,Mask,#Mask,#Cloth,!Mask,Neck,Offhand,Pack,Piercing,Rifle,Ring,Satchel,Scarf,Sidearm,Super Mutant,Mutant,Top,Vest,Armor,Clothing,A,C,Underarmor,Signal,Resource,Scrap,Tool,Currency,Lockpick,Utility,KW,KH,KR,KE,SkillA,SkillB,SkillTA,SkillC,SkillE1,SkillE2,SkillE3,SkillF,SkillH1,SkillH2,SkillL1,SkillL2,SkillM,SkillS1,SkillS2,SkillS3,SkillS4,SkillTR,SkillW1,SkillW2,SkillW3,SkillW4,SkillW5,WS,SkillX,SkillY,#Karma1,#Karma2,#Karma3,#HZ,Skill,Unique,Valuable,Collectible,Crafting,Craft,Mat,Misc,Game,Hack,Holotape,#Holotape,HoloC,#H,#Note,NoteC,#N,RE,#Fn,#FnM,#FnB,#FnR,#FnI,ADR,ADP,FSA,FSS,FSD,FSM,FSX,FSC,FST,FSE,FSI,STR,Comp,#Comp,BH,Mag,MagC,Quest,Trash,Key,KeyC,Pass,Mod,Mod2,Mod-A,Mod-W,Mod-AG,WAmmo,Mod-P,Mod-R,Mod-D,1star,1aim,1gun,1energy,1rifle,1melee,1armor,1helm,1skull,1atom,!A,1power,Grenade,GrenadeP,GrenadeE,GrenadeM,GrenadeK,Fire,Damage,Rad,Poison,Cryo,Mine,MineC,MineP,MineE,Trap,Flare,FlareS,FlareG,FlareV,FlareA,BPS,EL,EP,EPF,EWC,EWG,WM,WF,BG,BGA,BP,BR,BS,SNA,SNGB,SNGE,SNM,M,XM,XM1B,XM1S,XM2B,XM2S,XMU,Wrist,NPC,Shroud,Casual,Dress,Formal,Rugged,Lab,Fireworks,Settings,Check,1Settings,Modbook,Config,Menu,Nuke,VNuke,SW,SA,SC,SH,SE,SM,SP,RR,MM,Institute,BOS,BMarket,Gear,!Settler,Settler,Farm,Camp,Boat,City,Outpost,House,Store,Town,Foundation,Warehouse,Factory,Bunker,Airship,Aircraft,Hall,Bridge,Dock,Tower,Improvised,Natural,Park,Car,Radar,Water,Defense,Pipe,MG,Laser,Plasma,Missile,Shotgun,Gauss,Mining,Salvaging,Ore,Color,Display,Size,Weave,Pip,Filter';
    end;

    procedure removeAllConditions(e: IInterface);
    var
        conds: IInterface;
    begin
        conds := ElementByName(e, 'Conditions');
        if(assigned(conds)) then begin
            removeElement(e, conds);
        end;
    end;

    procedure setConditionToLocKeyword(ctda: IInterface; theKeyword: IInterface; orCond: boolean);
    //var
    begin
        if(orCond) then begin
            SetElementEditValues(ctda, 'CTDA\Type', '10010000');
        end else begin
            SetElementEditValues(ctda, 'CTDA\Type', '10000000');
        end;
        SetElementEditValues(ctda, 'CTDA\Comparison Value - Float', '1');
        SetElementEditValues(ctda, 'CTDA\Function', 'LocationHasKeyword');
        SetElementEditValues(ctda, 'CTDA\Run On', 'Subject');

        SetElementEditValues(ctda, 'CTDA\Keyword', IntToHex(GetLoadOrderFormID(theKeyword), 8));


    end;

    procedure setConditionToPerk(ctda: IInterface; thePerk: IInterface; orCond: boolean);
    //var
    begin
        if(orCond) then begin
            SetElementEditValues(ctda, 'CTDA\Type', '10010000');
        end else begin
            SetElementEditValues(ctda, 'CTDA\Type', '10000000');
        end;
        SetElementEditValues(ctda, 'CTDA\Comparison Value - Float', '1');
        SetElementEditValues(ctda, 'CTDA\Function', 'HasPerk');
        SetElementEditValues(ctda, 'CTDA\Run On', 'Reference');

        SetElementEditValues(ctda, 'CTDA\Reference', IntToHex(20, 8));//player
        SetElementEditValues(ctda, 'CTDA\Perk', IntToHex(GetLoadOrderFormID(thePerk), 8));//player

    end;

    procedure addConditionLocKeyword(e: IInterface; theKw: IInterface; orCond: boolean);
    var
        conds: IInterface;
        newEntry: IInterface;
    begin
        conds := ElementByName(e, 'Conditions');
        if(not assigned(conds)) then begin

            // this adds an entry automatically
            conds := add(e, 'Conditions', True);
            //AddMessage('foo '+ElementCount(ElementByName(conds, 'Condition')));
            newEntry := ElementByIndex(conds, 0);
            setConditionToLocKeyword(newEntry, theKw, orCond);

            //dumpElement(newEntry, '');
            //AddMessage(signature(newEntry));
        end else begin

            newEntry := ElementAssign(conds, HighInteger, nil, False);
            setConditionToLocKeyword(newEntry, theKw, orCond);
            //dumpElement(newEntry, '');
        end;


    end;

    procedure addConditionPerk(e: IInterface; thePerk: IInterface; orCond: boolean);
    var
        conds: IInterface;
        newEntry: IInterface;
    begin

        // add masters
        addRequiredMastersSilent(thePerk, toFile);


        conds := ElementByName(e, 'Conditions');
        if(not assigned(conds)) then begin

            // this adds an entry automatically
            conds := add(e, 'Conditions', True);
            //AddMessage('foo '+ElementCount(ElementByName(conds, 'Condition')));
            newEntry := ElementByIndex(conds, 0);
            setConditionToPerk(newEntry, thePerk, orCond);

        end else begin

            newEntry := ElementAssign(conds, HighInteger, nil, False);
            setConditionToPerk(newEntry, thePerk, orCond);
        end;


    end;

    procedure doSetup();
    var
        mainFileKYWD, architektKYWD, architectQUST: IInteface;
    begin
        architektKYWD := GroupBySignature(architectFile, 'KYWD');
        architectQUST := GroupBySignature(architectFile, 'QUST');
        mainFileKYWD := GroupBySignature(mainFile, 'KYWD');

        workbenchChemlabKw  := MainRecordByEditorID(mainFileKYWD, 'WorkbenchChemlab');
        utilityCategoryKw   := MainRecordByEditorID(mainFileKYWD, 'RecipeUtility');
        utility2CategoryKw  := MainRecordByEditorID(mainFileKYWD, 'RecipeMisc');

        perkArmorer1        := MainRecordByEditorID(GroupBySignature(mainFile, 'PERK'), 'Armorer01'); // perk Armorer01
        perkBlacksmith1     := MainRecordByEditorID(GroupBySignature(mainFile, 'PERK'), 'Blacksmith01'); // perk Blacksmith01
        //perkScience1        := MainRecordByEditorID(GroupBySignature(mainFile, 'PERK'), 'Science01'); // perk Science01
        //perkGunNut1         := MainRecordByEditorID(GroupBySignature(mainFile, 'PERK'), 'GunNut01'); // perk GunNut01



        //  benches and cats come from architect now
        // horizon benches
        WorkbenchWeaponsLab  := MainRecordByEditorID(architektKYWD, 'WorkbenchWeaponsLab_Arc');
        WorkbenchRoboticsLab := MainRecordByEditorID(architektKYWD, 'WorkbenchTechLab_Arc');

        // horizon cats, or rather architect
        RecipeIngredients    := MainRecordByEditorID(architektKYWD, 'RecipeIngredients_Arc'); //ingreds
        RecipeRobotics01            := MainRecordByEditorID(architektKYWD, 'RecipeTech_Devices'); // devices
        RecipeRobotics02     := MainRecordByEditorID(architektKYWD, 'RecipeTech_ToolsAndParts'); // tools and parts
        RecipeRobotics_Weapons      := MainRecordByEditorID(architektKYWD, 'RecipeWeaponLab_Weapons'); // weapons;
        RecipeRobotics_AmmoSpecial  := MainRecordByEditorID(architektKYWD, 'RecipeWeaponLab_AmmoExt_Arc'); // ammo special;

        BuildManager    := MainRecordByEditorID(architectQUST, 'Z_BuildManager');
        ResourceManager := MainRecordByEditorID(architectQUST, 'Z_ResourceManager');
        WorkshopManager := MainRecordByEditorID(architectQUST, 'Z_WorkshopManager');

        // vanilla cats
        RecipeGrenade  := MainRecordByEditorID(mainFileKYWD, 'RecipeGrenade');
        RecipeMines    := MainRecordByEditorID(mainFileKYWD, 'RecipeMines');


        capsItem := MainRecordByEditorID(GroupBySignature(mainFile, 'MISC'), 'Caps001');

        // these are still from horizon
        vendorContract  := MainRecordByEditorID(GroupBySignature(horizonFile, 'MISC'), 'AVendorContract');
        vendorCredit    := MainRecordByEditorID(GroupBySignature(horizonFile, 'MISC'), 'AVendorCurrency');

        // components and similar
        // from vanilla
        c_Fertilizer     := MainRecordByEditorID(GroupBySignature(mainFile, 'CMPO'), 'c_Fertilizer');
        c_Leather        := MainRecordByEditorID(GroupBySignature(mainFile, 'CMPO'), 'c_Leather');
        c_Cloth          := MainRecordByEditorID(GroupBySignature(mainFile, 'CMPO'), 'c_Cloth');
        c_Glass          := MainRecordByEditorID(GroupBySignature(mainFile, 'CMPO'), 'c_Glass');
        Scissors         := MainRecordByEditorID(GroupBySignature(mainFile, 'MISC'), 'Scissors');
        SurgicalScalpel  := MainRecordByEditorID(GroupBySignature(mainFile, 'MISC'), 'SurgicalScalpel');
        SurgicalScissors := MainRecordByEditorID(GroupBySignature(mainFile, 'MISC'), 'SurgicalScissors');
        c_Antiseptic     := MainRecordByEditorID(GroupBySignature(mainFile, 'CMPO'), 'c_Antiseptic');

        // from architect
        c_LiquidFuel        := MainRecordByEditorID(GroupBySignature(architectFile, 'CMPO'), 'c_LiquidFuel');
        c_PipeParts         := MainRecordByEditorID(GroupBySignature(architectFile, 'CMPO'), 'c_WeaponPartsPipe');
        c_PistolParts       := MainRecordByEditorID(GroupBySignature(architectFile, 'CMPO'), 'c_WeaponPartsPistol');
        c_ShotgunParts      := MainRecordByEditorID(GroupBySignature(architectFile, 'CMPO'), 'c_WeaponPartsShotgun');
        c_LaserParts        := MainRecordByEditorID(GroupBySignature(architectFile, 'CMPO'), 'c_WeaponPartsEnergy');
        c_ExplosiveMaterial := MainRecordByEditorID(GroupBySignature(architectFile, 'CMPO'), 'c_ExplosiveMaterial');
        ToolKit             := MainRecordByEditorID(GroupBySignature(architectFile, 'CMPO'), 'c_ToolKit_Common');// c_ToolKit_Common //c_ToolKit_Common "Common Tool" [CMPO:0800333D]
        ToolKitCooking      := MainRecordByEditorID(GroupBySignature(architectFile, 'CMPO'), 'c_ToolKit_Kitchen');// c_ToolKit_Common //c_ToolKit_Common "Common Tool" [CMPO:0800333D]
        ToolKitPrescision   := MainRecordByEditorID(GroupBySignature(architectFile, 'CMPO'), 'c_ToolKit_Precision');// c_ToolKit_Common //c_ToolKit_Common "Common Tool" [CMPO:0800333D]

        //Item_TurretKit_Arc  := MainRecordByEditorID(GroupBySignature(architectFile, 'MISC'), 'Item_TurretKit_Arc');// maybe c_WeaponPartsRifle "(  ) Rifle Weapon Parts" [CMPO:08002FF4] //c_WeaponPartsEnergy "(  ) Energy Weapon Parts" [CMPO:08002FF0]

        // horizon skills (actually perks)
        skillScience03      := MainRecordByEditorID(GroupBySignature(horizonFile, 'PERK'), 'Z_Skill_Science03');
        skillBallistics02   := MainRecordByEditorID(GroupBySignature(horizonFile, 'PERK'), 'Z_Skill_Ballistics02');
        skillLeadership01   := MainRecordByEditorID(GroupBySignature(horizonFile, 'PERK'), 'Z_Skill_Leadership01');
        skillLeadership02   := MainRecordByEditorID(GroupBySignature(horizonFile, 'PERK'), 'Z_Skill_Leadership02');
        skillTrading01      := MainRecordByEditorID(GroupBySignature(horizonFile, 'PERK'), 'Z_Skill_Trading01');
        skillTrading02      := MainRecordByEditorID(GroupBySignature(horizonFile, 'PERK'), 'Z_Skill_Trading02');

        Z_PerkSettlerTechnology02      := MainRecordByEditorID(GroupBySignature(architectFile, 'PERK'), 'Z_PerkSettlerTechnology02');
        Z_PerkSettlerTechnology03      := MainRecordByEditorID(GroupBySignature(architectFile, 'PERK'), 'Z_PerkSettlerTechnology03');
        Z_PerkSettlerTechnology04      := MainRecordByEditorID(GroupBySignature(architectFile, 'PERK'), 'Z_PerkSettlerTechnology04');
        Z_PerkSettlerTechnology05      := MainRecordByEditorID(GroupBySignature(architectFile, 'PERK'), 'Z_PerkSettlerTechnology05');

        // AVs
        avWaterProd         := MainRecordByEditorID(GroupBySignature(exeFile, 'AVIF'), 'Water');
        avPowerReq          := MainRecordByEditorID(GroupBySignature(exeFile, 'AVIF'), 'PowerRequired');
        avHappinessProd     := MainRecordByEditorID(GroupBySignature(mainFile, 'AVIF'), 'WorkshopRatingBonusHappiness');

        // spells
        //WaterFountainCleanDrinking "Cool, delicious, refreshing water" [SPEL:000B1ECD]
        spellWaterClean := MainRecordByEditorID(GroupBySignature(mainFile, 'SPEL'), 'WaterFountainCleanDrinking');
        //WaterRadiationDrinking "Cool, delicious, refreshing water" [SPEL:00024FBF]
        spellWaterRads := MainRecordByEditorID(GroupBySignature(mainFile, 'SPEL'), 'WaterRadiationDrinking');

        // item type keywords, all from architect
        // WorkbenchWeaponsLab  := MainRecordByEditorID(architektKYWD, 'WorkbenchWeaponsLab_Arc');
        ObjectTypeUtility       := MainRecordByEditorID(architektKYWD, 'ObjectTypeUtility');
        ObjectTypeBook          := MainRecordByEditorID(architektKYWD, 'ObjectTypeBook');
        ObjectTypeBuff          := MainRecordByEditorID(architektKYWD, 'ObjectTypeBuff');
        ObjectTypeCure          := MainRecordByEditorID(architektKYWD, 'ObjectTypeCure');
        ObjectTypeCurrency      := MainRecordByEditorID(architektKYWD, 'ObjectTypeCurrency');
        ObjectTypeDevice        := MainRecordByEditorID(architektKYWD, 'ObjectTypeDevice');
        ObjectTypeFoodClean     := MainRecordByEditorID(architektKYWD, 'ObjectTypeFoodClean');
        ObjectTypeFoodPrewar    := MainRecordByEditorID(architektKYWD, 'ObjectTypeFoodPrewar');
        ObjectTypeFoodRaw       := MainRecordByEditorID(architektKYWD, 'ObjectTypeFoodRaw');
        ObjectTypeJunkNoValue   := MainRecordByEditorID(architektKYWD, 'ObjectTypeJunkNoValue');
        ObjectTypeJunkCommon    := MainRecordByEditorID(architektKYWD, 'ObjectTypeJunkCommon');
        ObjectTypeJunkCommonLow := MainRecordByEditorID(architektKYWD, 'ObjectTypeJunkCommonLow');
        ObjectTypeJunkOrganic   := MainRecordByEditorID(architektKYWD, 'ObjectTypeJunkOrganic');
        ObjectTypeJunkQuality   := MainRecordByEditorID(architektKYWD, 'ObjectTypeJunkQuality');
        ObjectTypeMedical       := MainRecordByEditorID(architektKYWD, 'ObjectTypeMedical');
        ObjectTypeSellable      := MainRecordByEditorID(architektKYWD, 'ObjectTypeSellable');
        ObjectTypeShipment      := MainRecordByEditorID(architektKYWD, 'ObjectTypeShipment');
        ObjectTypeWaterClean    := MainRecordByEditorID(architektKYWD, 'ObjectTypeWaterClean');
        ObjectTypeJunkLegendary := MainRecordByEditorID(architektKYWD, 'ObjectTypeJunkLegendary');

        ObjectTypeQuest         := MainRecordByEditorID(architektKYWD, 'ObjectTypeQuest');
        ObjectTypeSellable      := MainRecordByEditorID(architektKYWD, 'ObjectTypeSellable');
        ObjectTypeSellableCollectable := MainRecordByEditorID(architektKYWD, 'ObjectTypeSellableCollectable');

        //ObjectTypeChem          := MainRecordByEditorID(mainFileKYWD, 'ObjectTypeChem');
        //ObjectTypeAlcohol       := MainRecordByEditorID(mainFileKYWD, 'ObjectTypeAlcohol');



        setupDone := true;
    end;

    function getBenchType(e: IInterface): string;
    var
        wbdt: IINterface;
        foo: IInterface;
    begin
        wbdt := ElementBySignature(e, 'WBDT');
        Result := '';
        if(assigned(wbdt)) then begin
            foo := ElementByName(wbdt, 'Bench Type');
            Result := GetEditValue(foo);
        end;
    end;

    function hasComponents(e: IInterface): boolean;
    var
        componentRoot: IInterface;
        component: IInterface;
        numComponents: integer;
    begin
        Result := false;
        componentRoot := ElementBySignature(e, 'CVPA');
        if assigned(componentRoot) then begin
            numComponents := ElementCount(componentRoot);
            if(numComponents > 0) then begin
                Result := true;
            end;
        end;
    end;


    procedure replaceFvpa(cobj: IInterface; search: IInterface; replace: IInterface; factor: float);
    var
        count: integer;
    begin
        count := getFvpaCount(cobj, search);
        if(count > 0) then begin
            setFvpaCount(cobj, search, 0);
            setFvpaCount(cobj, replace, count * factor);
        end;
    end;

    function isTurret(e: IInterface): boolean;
    var
        npcClass: IInterface;
        npcClassEdid: string;
        safetyProd: float;
    begin
        Result := false;
        npcClass := LinksTo(ElementByName(e, 'CNAM - Class'));
        if(not assigned(npcClass)) then begin
            exit;
        end;

        npcClassEdid := EditorID(npcClass);


        if ((npcClassEdid <> 'TurretBubbleClass') and (npcClassEdid <> 'TurretTripodClass')) then begin
            exit;
        end;

        safetyProd := getActorValuePropertyByName(e, 'Safety');
        if(safetyProd > 0) then begin
            Result := true;
            exit;
        end;

    end;

    function processActCobj(createdObj: IInterface; e: IInterface): IInterface;
    var
        safetyProd, powerReq: float;
        numTurretKits: integer;
        newElem: IInterface;
    begin
        Result := nil;
        powerReq := getActorValuePropertyByName(createdObj, 'PowerRequired');
        safetyProd := getActorValuePropertyByName(createdObj, 'Safety');

        numTurretKits := floor(safetyProd / 2);
        if (numTurretKits < 1) then begin
            numTurretKits := 1;
        end;

        newElem := createElementOverride(e, ToFile);
        removeAllConditions(newElem);

        //addRequiredMastersSilent(Item_TurretKit_Arc, ToFile, False);
        //setFvpaCount(newElem, Item_TurretKit_Arc, numTurretKits);

        if(powerReq <= 0) then begin
            setFvpaCount(newElem, c_LiquidFuel, 2);
        end;



        if (safetyProd >= 15) then begin
            addConditionPerk(newElem, Z_PerkSettlerTechnology05, false);
        end else if(safetyProd >= 10) then begin
            addConditionPerk(newElem, Z_PerkSettlerTechnology04, false);
        end else if(safetyProd >= 7) then begin
            addConditionPerk(newElem, Z_PerkSettlerTechnology03, false);
        end else if(safetyProd >= 5) then begin
            addConditionPerk(newElem, Z_PerkSettlerTechnology02, false);
        end;
        // c_LiquidFuel


        Result := newElem;
    end;


    function processFurnCobj(createdObj: IInterface; e: IInterface): IInterface;
    var
        benchType: String;
        newElem: IInterface;
        vendorIncome: float;
        vendorType: integer;
        vendorLevel: integer;
    begin
        // is it a workbench?
        if (hasKeywordByPath(createdObj, 'Workbench_General', 'KWDA')) then begin

            benchType := getBenchType(createdObj);
            if (
                (benchType = 'Weapons') or
                (benchType = 'Armor') or
                (benchType = 'Power Armor') or
                (
                    (benchType = 'Alchemy') and (
                        hasKeywordByPath(createdObj, 'WorkbenchCooking', 'KWDA') or
                        hasKeywordByPath(createdObj, 'WorkbenchChemlab', 'KWDA')
                    )
                )
            ) then begin


                { addRequiredMastersSilent(e, ToFile, False); }
                { newElem := wbCopyElementToFile(e, ToFile, False, True); }
                newElem := createElementOverride(e, ToFile);
                removeAllConditions(newElem);

                if(benchType = 'Alchemy') then begin
                    // for cooking, nothing
                    if(hasKeywordByPath(createdObj, 'WorkbenchChemlab', 'KWDA')) then begin
                        // for chemistry, 2 basic toolkits
                        // 1 presc TK c_ToolKit_Precision "(X) Precision Tool" [CMPO:08003408]
                        setFvpaCount(newElem, ToolKitPrescision, 2);
                    end;

                    if(hasKeywordByPath(createdObj, 'WorkbenchCooking', 'KWDA')) then begin
                        // cooking: 2x c_ToolKit_Kitchen "(X) Kitchen Tool" [CMPO:0800341B]
                        setFvpaCount(newElem, ToolKitCooking, 2);

                    end;

                end else if(benchType = 'Weapons') then begin
                    //addConditionPerk(newElem, SkillBallistics02, false);

                    // add 4 toolkits
                    setFvpaCount(newElem, ToolKit, 2);
                end else if(benchType = 'Armor') then begin
                    //addConditionPerk(newElem, perkArmorer1, false);
                    // add 4 toolkits
                    setFvpaCount(newElem, ToolKit, 2);

                end else if(benchType = 'Power Armor') then begin
                    // addConditionPerk(newElem, SkillScience03, false);
                    // addConditionPerk(newElem, perkBlacksmith1, false);
                    // add 2 toolkits
                    setFvpaCount(newElem, ToolKit, 2);
                end;
                Result := newElem;
            end;

            exit; // stop here
        end;

        // or a shop?
        if(hasScriptMulti(createdObj, 'workshopobjectscript', false)) then begin

            vendorType := getScriptPropertyInt(createdObj, 'VendorType', -1);
            vendorLevel := getScriptPropertyInt(createdObj, 'vendorLevel', 0);


            if(vendorType >= 0) and (vendorType < 6) then begin

                addRequiredMastersSilent(capsItem, ToFile);
                addRequiredMastersSilent(vendorContract, ToFile);
                addRequiredMastersSilent(e, ToFile);
                // newElem := wbCopyElementToFile(e, ToFile, False, True);
                newElem := createElementOverride(e, ToFile);
                removeAllConditions(newElem);

                // I think the vendor level is irrelevant now?
                // all require contract, none requires caps
                // all require 2 perks, but different one

                setFvpaCount(newElem, capsItem, 0);
                removeAllConditions(newElem);
                setFvpaCount(newElem, vendorContract, 1);

                case (vendorType) of
                    0:  begin
                            // generic
                            // Z_Skill_Leadership01 "Skill: Leadership (10)" [PERK:96123D5F]
                            // Z_Skill_Trading01 "Skill: Trading (10)" [PERK:96123D6E]
                            addConditionPerk(newElem, SkillLeadership01, false);
                            addConditionPerk(newElem, SkillTrading01, false);
                        end;
                    1:  begin
                            // armor
                            // Z_Skill_Leadership01 "Skill: Leadership (10)" [PERK:96123D5F]
                            // Z_Skill_Trading02 "Skill: Trading (20)" [PERK:96123D6F]
                            addConditionPerk(newElem, SkillLeadership01, false);
                            addConditionPerk(newElem, SkillTrading02, false);

                            // also cloth and leather
                            // 8x c_Leather "(  ) Leather" [CMPO:0001FAAE]
                            // 8x c_Cloth "(  ) Cloth" [CMPO:001223C7]
                            setFvpaCount(newElem, c_Leather, 8);
                            setFvpaCount(newElem, c_Cloth, 8);
                        end;
                    2:  begin
                            // weapon
                            // Z_Skill_Leadership01 "Skill: Leadership (10)" [PERK:96123D5F]
                            // Z_Skill_Trading02 "Skill: Trading (20)" [PERK:96123D6F]

                            addConditionPerk(newElem, SkillLeadership01, false);
                            addConditionPerk(newElem, SkillTrading02, false);

                            // also parts
                            // 6x c_LaserParts "(  ) Energy Weapon Parts" [CMPO:9600080B]
                            // 4x c_ShotgunParts "(  ) Shotgun Weapon Parts" [CMPO:96000FB6]
                            // 6x c_PistolParts "(  ) Pistol Weapon Parts" [CMPO:96000FBA]
                            // 6x c_PipeParts "(  ) Pipe Weapon Parts" [CMPO:96000FC3]
                            setFvpaCount(newElem, c_LaserParts, 6);
                            setFvpaCount(newElem, c_ShotgunParts, 4);
                            setFvpaCount(newElem, c_PistolParts, 6);
                            setFvpaCount(newElem, c_PipeParts, 6);
                        end;
                    3:  begin
                            // food
                            // Z_Skill_Leadership01 "Skill: Leadership (10)" [PERK:96123D5F]
                            // Z_Skill_Trading01 "Skill: Trading (10)" [PERK:96123D6E]

                            addConditionPerk(newElem, SkillLeadership01, false);
                            addConditionPerk(newElem, SkillTrading01, false);

                            setFvpaCount(newElem, c_Glass, 10);
                            setFvpaCount(c_Cloth, c_Glass, 2);
                            // 10x c_Glass "(  ) Glass" [CMPO:0001FAA4]
                            // 2x c_Cloth "(  ) Cloth" [CMPO:001223C7]
                        end;
                    4:  begin
                            // clinic
                            // Z_Skill_Trading01 "Skill: Trading (10)" [PERK:96123D6E]
                            // Z_Skill_Leadership02 "Skill: Leadership (20)" [PERK:96123D60]
                            addConditionPerk(newElem, SkillLeadership02, false);
                            addConditionPerk(newElem, SkillTrading01, false);

                            // 8x c_Antiseptic "(  ) Antiseptic" [CMPO:0001FA96]
                            // 1x SurgicalScissors "(X) Scissors (Surgical)" [MISC:000C9AD7]
                            // 1x SurgicalScalpel "(X) Scalpel (Surgical)" [MISC:000C9ADA]
                            // 10x c_Cloth "(  ) Cloth" [CMPO:001223C7]
                            setFvpaCount(newElem, c_Antiseptic, 8);
                            setFvpaCount(newElem, SurgicalScissors, 1);
                            setFvpaCount(newElem, SurgicalScalpel, 1);
                            setFvpaCount(newElem, c_Cloth, 10);

                        end;
                    5:  begin
                            // cloth
                            // Z_Skill_Leadership01 "Skill: Leadership (10)" [PERK:96123D5F]
                            // Z_Skill_Trading02 "Skill: Trading (20)" [PERK:96123D6F]
                            addConditionPerk(newElem, SkillLeadership01, false);
                            addConditionPerk(newElem, SkillTrading02, false);

                            // 6x c_Leather "(  ) Leather" [CMPO:0001FAAE]
                            // 1x Scissors "(X) Scissors" [MISC:00059B44]
                            // 12x c_Cloth "(  ) Cloth" [CMPO:001223C7]
                            setFvpaCount(newElem, c_Cloth, 12);
                            setFvpaCount(newElem, Scissors, 1);
                            setFvpaCount(newElem, c_Leather, 6);
                        end;
                end;// of case

                AddMessage('Adjusting recipe for '+DisplayName(createdObj));
                exit;
            end;


        end;


        exit;
    end;

    function processOtherCobj(createdObj: IInterface; e: IInterface): IInterface;
    var

        bnam: IInterface;
        createdSig: String;

        newElem: IInterface;
        newType: integer; // 1 : grenade, 2 : mine/trap, 3 : ammo, 4 : weapon, 5 : device, 6 : tool, 7 : ingredient
        alchType: integer;
    begin

        bnam := getBnam(e);
        newType := 0;
        createdSig := signature(createdObj);


        if(not isSameForm(bnam, workbenchChemlabKw)) then begin
            exit;
        end;

        if(createdSig = 'AMMO') then begin
            newType := 3;
        end else begin
            if (hasKeywordByPath(e, 'RecipeMines', 'FNAM') or hasKeywordByPath(e, 'RecipeThrowingTrap', 'FNAM')) then begin
                newType := 2;
            end else if (hasKeywordByPath(e, 'RecipeGrenade', 'FNAM')) then begin
                newType := 1;
            end;

        end;

        if(newType = 0) and (createdSig = 'NOTE') then begin
            newType := 5;
        end;

        if((newType = 0) and (createdSig = 'WEAP')) then begin
            newType := 4;
        end;

        if(newType = 0) then begin
            if(createdSig = 'ALCH') then begin
                alchType := getAlchemyType(createdObj);
                // leave consumables where they are
                case (alchType) of
                    40: newType := 5;
                    41: newType := 6;
                end;
            end else if(createdSig = 'MISC') then begin
                if(hasComponents(createdObj)) then begin
                    newType := 7;
                end;
            end;
        end;

        // AddMessage('NewType: '+IntToStr(newType));
         // 1 : grenade, 2 : mine/trap, 3 : ammo, 4 : weapon, 5 : device, 6 : tool, 7 : ingredient
        if(newType > 0) then begin
            //addRequiredMastersSilent(e, ToFile, False);
            addRequiredMastersSilent(WorkbenchWeaponsLab, ToFile);

            //newElem := wbCopyElementToFile(e, ToFile, False, True);
            newElem := createElementOverride(e, ToFile);
            case(newType) of
                1:  begin // grenade
                        // an explosive
                        // replace c_Fertilizer by c_ExplosiveMaterial
                        replaceFvpa(newElem, c_Fertilizer, c_ExplosiveMaterial, 10);
                        AddMessage('Moving '+DisplayName(createdObj)+' to the Weapons Lab');
                        setBnam(newElem, WorkbenchWeaponsLab);
                        replaceAnyFnam(newElem, replaceableFnamList, RecipeGrenade, true);
                    end;
                2:  begin // mine
                        replaceFvpa(newElem, c_Fertilizer, c_ExplosiveMaterial, 10);
                        AddMessage('Moving '+DisplayName(createdObj)+' to the Weapons Lab');
                        setBnam(newElem, WorkbenchWeaponsLab);
                        replaceAnyFnam(newElem, replaceableFnamList, RecipeMines, true);
                    end;
                3:  begin // ammo
                        AddMessage('Moving '+DisplayName(createdObj)+' to the Weapons Lab');
                        setBnam(newElem, WorkbenchWeaponsLab);
                        replaceAnyFnam(newElem, replaceableFnamList, RecipeRobotics_AmmoSpecial, true);
                    end;
                4:  begin // weapon
                        AddMessage('Moving '+DisplayName(createdObj)+' to the Weapons Lab');
                        setBnam(newElem, WorkbenchWeaponsLab);
                        replaceAnyFnam(newElem, replaceableFnamList, RecipeRobotics_Weapons, true);
                    end;
                5:  begin // device
                        AddMessage('Moving '+DisplayName(createdObj)+' to the Tech Lab');
                        setBnam(newElem, WorkbenchRoboticsLab);
                        replaceAnyFnam(newElem, replaceableFnamList, RecipeRobotics01, true);
                        // dumpElement(RecipeRobotics01, 'foo');
                    end;
                6:  begin // tool
                        AddMessage('Moving '+DisplayName(createdObj)+' to the Tech Lab');
                        setBnam(newElem, WorkbenchRoboticsLab);
                        replaceAnyFnam(newElem, replaceableFnamList, RecipeRobotics02, true);
                    end;
                6:  begin // ingred
                        AddMessage('Moving '+DisplayName(createdObj)+' to ingredients within the chemistry workbench');
                        replaceAnyFnam(newElem, replaceableFnamList, RecipeIngredients, true);
                    end;
            end;
            Result := newElem;
        end;

    end;

    function processWaterGenerator(e: IInterface): boolean;
    var
        waterProduction: float;
        powerReq: float;
        happinessProd: float;
        isAnythingGround: float;

        powerReqShouldBe: float;
        happinessProdShouldBe: float;

        newElem: IInterface;
    begin
        Result := false;
        // check if this produces water
        waterProduction := getActorValuePropertyByProp(e, avWaterProd);
        if ((waterProduction = 0) or (getActorValuePropertyByName(e, 'WorkshopResourceObject') = 0) or (not hasKeywordByPath(e, 'WorkshopCanBePowered', 'KWDA'))) then begin
            exit;
        end;

        Result := true;
        // horizon seems to do something like:
        // power req is half the water prod
        // happiness is twice the water prod
        // the atmospheric converter seems to be the exception, by requiring 75% power
        // atmospheric collector has "anything is ground"
        isAnythingGround := getActorValuePropertyByName(e, 'WorkshopAnythingIsGround');
        happinessProd := getActorValuePropertyByProp(e, avHappinessProd);

        // WorkshopCanBePowered [KYWD:0003037E]


        powerReq := getActorValuePropertyByProp(e, avPowerReq);

        happinessProdShouldBe := waterProduction*2;
        if(isAnythingGround > 0) then begin
            powerReqShouldBe := Ceil(waterProduction*0.75);
        end else begin
            powerReqShouldBe := Ceil(waterProduction*0.5);
        end;

        if(happinessProdShouldBe <> happinessProd or powerReqShouldBe <> powerReq) then begin
            AddMessage('Adjusting power/happiness of '+DisplayName(e));

            newElem := createElementOverride(e, ToFile);

            setActorValueProperty(newElem, avPowerReq, powerReqShouldBe);
            setActorValueProperty(newElem, avHappinessProd, happinessProdShouldBe);

        end;

        { avWaterProd: IInterface;
        avPowerReq: IInterface;
        avHappinessProd: IInterface;

        }

    end;

    function processDrinkingFountain(e: IInterface): boolean;
    var
        spellActi, spellFountain: IInterface;
        newElem: IInterface;
    begin
        result := false;

        // check if this is drinkable
        if(hasScriptMulti(e, 'drinkableActivatorScript', false) or hasScriptMulti(e, 'drinkFromFountainScript', false)) then begin
            result := true;

            spellActi := getPropertyOfScriptLinkedObject(e, 'drinkableActivatorScript', 'WaterFountainCleanDrinking');
            spellFountain := getPropertyOfScriptLinkedObject(e, 'drinkFromFountainScript', 'spellOnDrink');

            if (isSameForm(spellWaterClean, spellActi) or isSameForm(spellWaterClean, spellFountain)) then begin
                AddMessage('Changing water to dirty for '+DisplayName(e));
                newElem := createElementOverride(e, ToFile);

                if(assigned(spellFountain)) then begin
                    setPropertyOfScriptLinkedObject(newElem, 'drinkFromFountainScript', 'spellOnDrink', spellWaterRads);
                end;

                if(assigned(spellActi)) then begin
                    setPropertyOfScriptLinkedObject(newElem, 'drinkableActivatorScript', 'WaterFountainCleanDrinking', spellWaterRads);
                end;

            end;

        end;
    end;

    procedure processFurn(e: IInterface);
    var
        script, newElem: IInterface;
        vendorType: integer;
    begin
        if(not hasScriptMulti(e, 'workshopobjectscript', false)) then begin
            exit;
        end;
        // OMG what did I even...

        vendorType := getScriptPropertyInt(e, 'VendorType', -1);

        if(vendorType < 0) then begin
            exit;
        end;

        newElem := createElementOverride(e, ToFile);

        // rename the script
        script := GetScript(newElem, 'workshopobjectscript');
        SetElementEditValues(script, 'scriptName', 'Script_Z_BuildObjectUpgrade');

        setScriptProp(script, 'ResourceActivatedBy', 2);
        setScriptProp(script, 'ResourceType', 8);
        setScriptProp(script, 'SettlerTypesAllowed', 0);

        setScriptProp(script, 'BM', BuildManager);
        setScriptProp(script, 'RM', ResourceManager);
        setScriptProp(script, 'WM', WorkshopManager);

    end;

    procedure processActi(e: IInterface);
    var
        res: IInterface;
    begin
        if(processWaterGenerator(e)) then begin
            exit;
        end;

        processDrinkingFountain(e);
    end;

    function processCobj(e: IInterface): IInterface;
    var
        createdObj: IInterface;
        bnam: IInterface;
        createdSig: String;

        newElem: IInterface;
        newType: integer; // 1 : grenade, 2 : mine/trap, 3 : ammo, 4 : weapon, 5 : device, 6 : tool, 7 : ingredient
        alchType: integer;
    begin
        createdObj := getCraftResult(e);
        bnam := getBnam(e);
        newType := 0;
        createdSig := signature(createdObj);

        if(createdSig = 'FURN') then begin
            Result := processFurnCobj(createdObj, e);
            exit;
        end;

        if ((createdSig = 'NPC_') and (isTurret(createdObj))) then begin
            Result := processActCobj(createdObj, e);
            exit;
        end;

        Result := processOtherCobj(createdObj, e);

        { // postprocessing }
        { if (createdSig = 'ACTI') then begin }
            { processActi(createdObj); }
        { end; }
    end;

    function Initialize: integer;
    var
        dir: String;
        Path: String;
        pattern: String;
        Attr: Integer;
        FileAttr: Integer;
        Res: TSearchRec;
        Name: String;
    begin
        isSuccessful := true;
        setupDone := false;
        initTagifier();

        initTags();

        objectTypeKeywords := TStringList.create;
        objectTypeKeywords.add('ObjectTypeLooseMod');
        objectTypeKeywords.add('ObjectTypeAmmoPack');
        objectTypeKeywords.add('ObjectTypeBook');
        objectTypeKeywords.add('ObjectTypeBuff');
        objectTypeKeywords.add('ObjectTypeClothing');
        objectTypeKeywords.add('ObjectTypeCure');
        objectTypeKeywords.add('ObjectTypeCurrency');
        objectTypeKeywords.add('ObjectTypeDevice');
        objectTypeKeywords.add('ObjectTypeDogFood');
        objectTypeKeywords.add('ObjectTypeEyewear');
        objectTypeKeywords.add('ObjectTypeChem');
        objectTypeKeywords.add('ObjectTypeAmmo');
        objectTypeKeywords.add('ObjectTypeArmor');
        objectTypeKeywords.add('ObjectTypeWeapon');
        objectTypeKeywords.add('ObjectTypeStimpak');
        objectTypeKeywords.add('ObjectTypeDrink');
        objectTypeKeywords.add('ObjectTypeWater');
        objectTypeKeywords.add('ObjectTypeFoodClean');
        objectTypeKeywords.add('ObjectTypeFoodPrewar');
        objectTypeKeywords.add('ObjectTypeFoodRaw');
        objectTypeKeywords.add('ObjectTypeJunkCommonLow');
        objectTypeKeywords.add('ObjectTypeJunkCommonMedical');
        objectTypeKeywords.add('ObjectTypeJunkNoValue');
        objectTypeKeywords.add('ObjectTypeJunkOrganic');
        objectTypeKeywords.add('ObjectTypeJunkQuality');
        objectTypeKeywords.add('ObjectTypeMedical');
        objectTypeKeywords.add('ObjectTypePowerArmor');
        objectTypeKeywords.add('ObjectTypeQuest');
        objectTypeKeywords.add('ObjectTypeSellable');
        objectTypeKeywords.add('ObjectTypeSellableCollectable');
        objectTypeKeywords.add('ObjectTypeSharedRestricted');
        objectTypeKeywords.add('ObjectTypeIngredient');
        objectTypeKeywords.add('ObjectTypeVendorArmor');
        objectTypeKeywords.add('ObjectTypeVendorWeapon');
        objectTypeKeywords.add('ObjectTypeWaterClean');
        objectTypeKeywords.add('ObjectTypeWeaponPack');
        objectTypeKeywords.add('ObjectTypeJunkLegendary');
        objectTypeKeywords.add('ObjectTypeUtility');
        objectTypeKeywords.add('ObjectTypeMedicalT1');
        objectTypeKeywords.add('ObjectTypeMedicalT2');
        objectTypeKeywords.add('ObjectTypeMedicalT3');
        objectTypeKeywords.add('ObjectTypeAmmoAddT1');
        objectTypeKeywords.add('ObjectTypeAmmoAddT2');
        objectTypeKeywords.add('ObjectTypeAmmoAddT3');
        objectTypeKeywords.add('ObjectTypeAmmoAddT4');
        objectTypeKeywords.add('ObjectTypeAmmoAddT5');
        objectTypeKeywords.add('ObjectTypeExplosives');
        objectTypeKeywords.add('ObjectTypeThreatAddLow');
        objectTypeKeywords.add('ObjectTypeThreatAddMedium');
        objectTypeKeywords.add('ObjectTypeThreatAddHigh');
        objectTypeKeywords.add('ObjectTypeJunkCommon');
        objectTypeKeywords.add('ObjectTypeJunkCommonAmmo');
        objectTypeKeywords.add('ObjectTypeSchematic');
        objectTypeKeywords.add('ObjectTypeTriggerFire');


        replaceableFnamList := TStringList.create;
        replaceableFnamList.add('RecipeUtility');
        replaceableFnamList.add('RecipeMisc');
        // replaceableFnamList.add('RecipeHealing');
        // replaceableFnamList.add('RecipeGrenade');
        // replaceableFnamList.add('RecipeMines');

        effectsToRemove := TStringList.create;

        effectsToRemove.add('RestoreHealthFood');
        effectsToRemove.add('FortifyHealthAlcohol');
        // effectsToRemove.add('FortifyEnduranceAlcohol'); // Horizon doesn't remove it
        effectsToRemove.add('FortifyHealRate');
        effectsToRemove.add('RestoreHealthChem');

        // this seems to translate to "Fallout.exe" in fo4edit
        exeFile := findFile('Fallout4.exe');
        mainFile := findFile('Fallout4.esm');
        horizonFile := findFile('Z_Horizon.esp');
        architectFile := findFile('Z_Architect.esm');

        if(not assigned(exeFile)) then begin
            AddMessage('Fallout4.exe missing');
            isSuccessful := false;
        end;

        if(not assigned(mainFile)) then begin
            AddMessage('Fallout4.esm missing');
            isSuccessful := false;
        end;

        if(not assigned(horizonFile)) then begin
            AddMessage('Z_Horizon.esp missing');
            isSuccessful := false;
        end;

        if(not assigned(architectFile)) then begin
            AddMessage('Z_Architect.esm missing');
            isSuccessful := false;
        end;

        if(not isSuccessful) then begin
            Result := 1;
        end;
    end;

    procedure ensureKeyword(e: IInterface; keyword: IInterface);
    begin
        if(hasKeywordIInterface(e, keyword, 'KWDA')) then begin
            exit;
        end;

        addRequiredMastersSilent(keyword, getFile(e));
        addKeyword(e, keyword, 'KWDA');
    end;

    function applyKeywords(e: IInterface): integer;
    var
        sig: String;
        alchType: integer;
        weight, value: float;
        miscComponents: TStringList;
    begin
        Result := 0;

        if(hasAnyKeyword(e, objectTypeKeywords, 'KWDA')) then begin
            exit;
        end;
        sig := signature(e);

        {

        ObjectTypeJunkCommon: IInterface; //ObjectTypeJunkCommon [KYWD:0C002D28] also fiberglass, circuitry, allowys? value >= 10
        ObjectTypeJunkCommonLow : IInterface; // ObjectTypeJunkCommonLow [KYWD:0C002C71] generic junk? value < 10 maybe? glass, steel, wood, rubber, screw, packing material, plastic, ceramic, asbestos, spring, gear, aluminum, cloth

        }

        if(sig = 'MISC') then begin
            // possible:
            if(hasScriptMulti(e, 'Script_Z_ItemPackDeploy', false)) then begin
                // packed Horizon kit, no idea what else I can do here
                ensureKeyword(e, ObjectTypeUtility);
                exit;
            end;

            if hasScript(e, 'ShipmentScript') then begin
                ensureKeyword(e, ObjectTypeShipment);
                exit;
            end;

            weight := StrToFloat( GetElementEditValues(e, 'DATA\Weight') );
            value := StrToInt( GetElementEditValues(e, 'DATA\Value') );

            if(value = 0) then begin
                ensureKeyword(e, ObjectTypeJunkNoValue);
                exit;
            end;

            miscComponents := getComponentsList(e);

            if(miscComponents.count > 0) then begin
                // component-based stuff
                //
                if (miscComponents.indexOf('c_Leather') >= 0) or (miscComponents.indexOf('c_Bone') >= 0) then begin
                    ensureKeyword(e, ObjectTypeJunkOrganic);
                    miscComponents.free();
                    exit;
                end;

                if (value > 100) or
                    (miscComponents.indexOf('c_Gold') >= 0) or
                    (miscComponents.indexOf('c_Silver') >= 0) or
                    (miscComponents.indexOf('c_Circuitry') >= 0) or
                    (miscComponents.indexOf('c_Adhesive') >= 0) or
                    (miscComponents.indexOf('c_AlloyAluminum01') >= 0) or
                    (miscComponents.indexOf('c_AlloyAluminum02') >= 0) or
                    (miscComponents.indexOf('c_AlloyAluminum03') >= 0) or
                    (miscComponents.indexOf('c_AlloyAluminum04') >= 0) or
                    (miscComponents.indexOf('c_AlloyAluminum05') >= 0) or
                    (miscComponents.indexOf('c_AlloyAluminum06') >= 0) or
                    (miscComponents.indexOf('c_AlloyAluminum07') >= 0) or
                    (miscComponents.indexOf('c_AlloyAluminum08') >= 0) or
                    (miscComponents.indexOf('c_AlloyAluminum09') >= 0) or
                    (miscComponents.indexOf('c_AlloyAluminum10') >= 0) or
                    (miscComponents.indexOf('c_AlloySteel01') >= 0) or
                    (miscComponents.indexOf('c_AlloySteel02') >= 0) or
                    (miscComponents.indexOf('c_AlloySteel03') >= 0) or
                    (miscComponents.indexOf('c_AlloySteel04') >= 0) or
                    (miscComponents.indexOf('c_AlloySteel05') >= 0) or
                    (miscComponents.indexOf('c_AlloySteel06') >= 0) or
                    (miscComponents.indexOf('c_AlloySteel07') >= 0) or
                    (miscComponents.indexOf('c_AlloySteel08') >= 0) or
                    (miscComponents.indexOf('c_AlloySteel09') >= 0) or
                    (miscComponents.indexOf('c_AlloySteel10') >= 0) or
                    (miscComponents.indexOf('c_AlloyTitanium01') >= 0) or
                    (miscComponents.indexOf('c_AlloyTitanium02') >= 0) or
                    (miscComponents.indexOf('c_AlloyTitanium03') >= 0) or
                    (miscComponents.indexOf('c_AlloyTitanium04') >= 0) or
                    (miscComponents.indexOf('c_AlloyTitanium05') >= 0) or
                    (miscComponents.indexOf('c_AlloyTitanium06') >= 0) or
                    (miscComponents.indexOf('c_AlloyTitanium07') >= 0) or
                    (miscComponents.indexOf('c_AlloyTitanium08') >= 0) or
                    (miscComponents.indexOf('c_AlloyTitanium09') >= 0) or
                    (miscComponents.indexOf('c_AlloyTitanium10') >= 0) or
                    (miscComponents.indexOf('c_AlloyBeryllium01') >= 0) or
                    (miscComponents.indexOf('c_AnimalHide01') >= 0) or
                    (miscComponents.indexOf('c_AlloyMagnesium01') >= 0) or
                    (miscComponents.indexOf('c_AlloyNickel01') >= 0) or
                    (miscComponents.indexOf('c_AlloyTungsten01') >= 0) or
                    (miscComponents.indexOf('c_AlloyZirconium01') >= 0) or
                    (miscComponents.indexOf('c_AlloyChromium01') >= 0) then begin

                    ensureKeyword(e, ObjectTypeJunkQuality);
                    miscComponents.free();
                    exit;
                end;

                if(value < 10) then begin
                    ensureKeyword(e, ObjectTypeJunkCommonLow);
                    miscComponents.free();
                    exit;
                end;



            end else begin
                if hasKWDA(e, 'FeaturedItem') then begin
                    // sellable?
                    if(hasKWDA(e, 'VendorItemNoSale')) then begin
                        ensureKeyword(e, ObjectTypeQuest);
                    end else begin
                        ensureKeyword(e, ObjectTypeSellableCollectable);
                    end;
                end else begin
                    if ((weight = 0) and (value > 0)) then begin
                        // weightless but valuable -> currency
                        ensureKeyword(e, ObjectTypeCurrency);
                    end else if (value > 0) and (value >= weight) then begin
                        // has weight and valuable -> valuable
                        ensureKeyword(e, ObjectTypeSellable);
                    end else begin
                        // otherwise trash
                        ensureKeyword(e, ObjectTypeJunkCommonLow);
                    end;
                end;
            end;

            miscComponents.free();

            exit;
        end;

        if(sig = 'ALCH') then begin
            alchType := getAlchemyType(e);

            case alchType of
                1: ensureKeyword(e, ObjectTypeBuff);    // good chem
                //2: ensureKeyword(e, ObjectTypeChem);    // bad chem
                // food
                10: ensureKeyword(e, ObjectTypeFoodClean); // food
                11: ensureKeyword(e, ObjectTypeFoodRaw); // raw
                12: ensureKeyword(e, ObjectTypeFoodPrewar); // prewar
                13: ensureKeyword(e, ObjectTypeFoodRaw); // crop
                // drink
                //20: ensureKeyword(e, ObjectTypeFoodRaw); // other
                //21: ensureKeyword(e, ObjectTypeFoodRaw); // liquor
                //22: ensureKeyword(e, ObjectTypeFoodRaw); // nuka
                // syringe
                //30:
                // device
                40: ensureKeyword(e, ObjectTypeDevice); // device
                41: ensureKeyword(e, ObjectTypeDevice); // tool
            end;

            exit;
        end;
    end;

    function Process(e: IInterface): integer;
    var
      i: integer;
      frm: TForm;
      clb: TCheckListBox;
      curSig: String;
      newName: String;
      curName: String;
      scrapComponentsString: String;
      newElem: IInterface;
      curEdid: String;
    begin

        if(not isSuccessful) then begin
            Result := 1;
            exit;
        end;

        if(not setupDone) then begin
            doSetup();
        end;

        curSig := signature(e);
        if curSig = 'TES4' then
            Exit;



        if not Assigned(ToFile) then begin
            ToFile := showFileSelectionDialog(e);

            if not Assigned(ToFile) then begin
                Result := 1;
                Exit;
            end;
        end;

        curEdid := GetElementEditValues(e, 'EDID');
        if(IsItemIgnored(curEdid)) then begin
            AddMessage('Skipping '+curEdid);
            exit;
        end;

        BeginUpdate(e);
        try
            if(curSig = 'COBJ') then begin
                processCobj(e);
            end else if (curSig = 'FURN') then begin
                processFurn(e);
            end else if(curSig = 'ACTI') then begin
                processActi(e);
            end else begin

                // now actually do stuff
                newElem := tagifyElement(e, toFile);

                // do more
                if(curSig = 'ALCH') then begin
                    if(hasAnyEffect(e, effectsToRemove)) then begin
                        if(not assigned(newElem)) then begin
                            //addRequiredMastersSilent(e, ToFile, False);
                            //newElem := wbCopyElementToFile(e, ToFile, False, True);
                            newElem := createElementOverride(e, ToFile);
                        end;
                        AddMessage('Removing effects from: '+DisplayName(newElem));
                        removeEffects(newElem, effectsToRemove);
                    end;
                end;

                applyKeywords(newElem);
            end;
        finally
            EndUpdate(e);
        end;
    end;

    function Finalize: integer;
    begin
        Result := 0;
        cleanupTagifier();
    end;
end.