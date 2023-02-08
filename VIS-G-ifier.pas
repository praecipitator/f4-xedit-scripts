{
    Attempts to automatically figure out VIS-G tags for selected items and creates overrides for them with those tags.
    Additionally, text files can be put into Edit Scripts\tagging-overrides\ in the format of
        editor_id=[TAG]
    for manual renaming configuration

}
unit VisGIfier;

    //uses VisOverrides;
    uses praFunctions;
    uses praTagifier;

    var
        ToFile: IInterface;

    procedure initTags();
    begin
        settingAddComponentString := true;

        tagShipment     := '(Resource)';       // Shipments
        tagScrap        := '{Scrap}';       // Scrap, MISCs which contain components
        tagResource     := '(Resource)';    // Resources, MISCs which a meant to represent one type of component
        tagLooseMod     := '{Mod}';         // Loose modifications
        tagCollectible  := '[Collectible]'; // "Collectible" MISCs
        tagQuest        := '[Quest]';       // Quest items
        tagCurrency     := '(Currency)';    // "Currency", MISCs with zero weight and non-zero value
        tagValuable     := '(Valuable)';    // MISCs with more value than weight
        tagOtherMisc    := '[Trash]';       // all other MISCs, trash
        tagGoodChem     := '(Aid)';         // Cures etc
        tagBadChem      := '[Chem]';        // Addictive chems
        tagFood         := '(Food)';        // generic food, selfcooked. Usually radless
        tagFoodRaw      := '[Raw Meat]';    // raw food, has rads, has disease risk
        tagFoodCrop     := '[Produce]';     // crops, has rads
        tagFoodPrewar   := '[Rad Food]';    // prewar packaged, has rads
        tagDrink        := '(Drink)';       // generic drinkable
        tagLiquor       := '[Liquor]';      // Alcoholic beverages
        tagNukacola     := '(Nuka)';        // Nuka Cola of any kind
        tagSyringe      := '[Syringe]';     // Syringer ammo
        tagDevice       := '(Device)';      // Consumables which are supposed to be devices instead of something to eat, like the Stealth Boy
        tagTool         := '[Tool]';        // Similar to above, but for more low-tech things. Like SimSettlements Town Meeting Gavel, or the Companion Whistle
        tagNews         := '[Note]';        // Newspaper
        tagNote         := '[Note]';        // Any paper note
        tagPerkmag      := '[Perk: Mag]';   // Perk Magazine
        tagMine         := '(Mine)';        // Mine
        tagGrenade      := '(Grenade)';     // Grenade
        tagKey          := '|Key|';         // Keycard
        tagAmmo         := '(Ammo)';        // Generic Ammo
        tagHolotape     := '[Holotape]';    // Holotape
        tagHolotapeGame := '[Game]';
        tagHolotapeSettings := '[Settings]';
        tagPipBoy       := '(Pipboy)';
        // If any of the tags in this list, or any of the tags above are discovered (no matter the brackets), it will be considered "tagged", and skipped.
        extraValidTags  := 'Aid,Device,Drink,Food,Chem,Liquor,Nuka,Rad Food,Beer,Ingredients,Raw Meat,Produce,Egg,Plant,Fungi,Tea,Coffee,Trim,Hash,Pollen,Blunt,Joint,Seed,Buds,Magazine,Cigarette,Cigar,Canteen,Ammo,Fuel,Bolt,Fusion,mFusion,Syringe,Flare,1e Combat,1h Synth,1r DC Guard,1q Leather,1j Metal,1b Marine,1i Robot,1a Therm Optics,1p Trapper,1f Nano,1c Merc,1d Gunners,1o Scavvers,1g Battle,1k Disciples,1l Operators,1m Pack,1n Raider,1s Vault-Tec,3a Super Mutant,2a X-03,2b X-02,2c X-01,2d T-60,2e T-53,2f T-51c,2g T-51,2h T-49,2i T-45,2j Raider,2k Combat PA,2l Construction,2m Horse Power,2ma Cpt. Cosmos,2n Institute PA,2o Ironman,2p Knight,2q Liberty,2r Midwest BoS,2s Navi,2t Relic Marine,2u Space Marine,2v Spartan,2w Submersible,2x Teddy Bear,2y TES-51,2z Train,2za Tribal,2zb Vault-Tec,1a Silver Shroud,1b Robot,1c Biosuit,1d Suit,1e Dress,1f Dapper,1g Casual,1h Skimpy,1i Rugged,1j Minutemen,1k Vault-Tec,1l Atom Cats,1m BoS,1n Railroad,1o Institute,1p DC,1q Military,1r Marine,1s NCR,1t Merc,1ta Gunner,1tb Scavvers,1u Forged,1v Disciples,1w Operators,1x Pack,1y Raider,1z CoA,2a Masked Helmet,2b Helmet,2c Hat,2d Cap,2e Headband,2f Full-Mask,2g Eyewear,2h Mouth,2i Half-Mask,2j Bandana,2k Scarf,2l Bandana,2m Gloves,3a Backpack,3b Jetpack,3c Backpack Upper,3d Backpack Lower,3e Satchel,3f Bandolier,3g Harness,3h Tac-Vest,3i Belt,4a Gun On Hip,4b Melee On Hip,4c Gun On Back,4d Melee On Back,4e Canteen,4f Device,4g Mine,5a Necklace,5b Bracelet,5c Watch,5d Ring,5e Earring,5f Piercing,5g Fingernails,6a Top,6b Bottom,6c Sneakers,6d Boots,6e Jacket,6f Vest,6g Cloak,6h Arm Addon,6i Leg Addon,6j Offhand,7a Dog,8a Kids Dress,8b Kids Casual,8c Kids Rugged,8d Kids Vault-Tec,8e Kids BoS,8f Kids Institute,8g Kids Military,8h Kids Cap,Explosives,Grenade,Mine,Signal,Beacon,Nuke Grenade,Pulse Grenade,Stun Grenade,Flash Grenade,Dynamite,Holy Grenade,Molotov,Thrown,Resource,Scrap,Ammo Scrap,Resource AGP,Resource AP,Currency,Lockpick,Settings,Unique,Valuable,Bottle,Collectibles,Collectible,Crafting,CC Kits,CC Sets,Game,Hack,Holotape,Note,Quest,Trash,Compost,Key,Password,Passcard,Camping,Pipboy,Wraps,Tool,Casing,Shell,Baseball Card,Marvel Card,Coffee Cup,Gnome,Model Robot,Cpt. Cosmos,GI Joe,Mod,Perk: Quest,Perk: Companion,Faction: Railroad,Faction: BOS,Perk: Bobblehead,Perk: Mag,Faction: Operator,Faction: Disciple,Faction: Pack,Faction: Atom Cats,0a Mini Nuke,0b Missile,0c RocketsMini,0c RocketsCluster,0c Fireworks,0c RocketsLrg,0c Kids Rocket,0d 40mm Grenade,0e Cannonball,1a 2mm EC,1b Alien,1b ET,1ba SC,1c PR,1d PP,1e Core,1f Tesla,1g Cryo,1h Flamer,1i IR,1j IP,1k LR,1l LP,1m LM,1n Annie Boom,1o Gamma,1p Breeder,1q Assaultron Head,2a HMG,2b LMG,2c BR,2d AR,2da HAR,2e SAR,2f SAS,2g SG,2h LAR,2i HMR,2j CARB,2k SMG,2ka CSMG,2kb HSMG,2la HR,2lb REV,2m HP,2n SAP,2o LSAP,2p HMP,3a Harpoon,3b Spike,3c Bolt,3d Arrow,3e Syringer,3f Baseball,3g Melon,3h Junk,3i Flare,3j Acid,3k Zapper,3l Paintball,4aa Super Sledge,4ab Sledgehammer,4ba Chainsaw,4bb Buzz Axe,4bc Buzz Blade,4bd Ripper,4c Fire,4d Sword,4da Katana,4db Assaultron Blade,4dd Jiang,4de Saber,4e Axe,4f Bat,4fb Pipe Wrench,4fc Lead Pipe,4fd Board,4fe Pole Hook,4ff Pool Cue,4fg Baton,4fh Tire Iron,4fi Rolling Pin,4fj Walking Cane,4fk Club,4ga Hachet,4gb Tomahawk,4h Machete,4i Knife,4ia Trench Knife,4ib Switchblade,4ic Karambit,4j Fist,4ja Power Fist,4jb Deathclaw Gauntlet,4jc Meat Hook,4jd Knuckles,4je Push Knife,4jf Boxing Gloves,4jz Melee,4k String,1h 1 Fahrenheit_Flamer,1i 1 Virgils_Rifle,1l 1 Survivors_Special,2e 1 Reba,2o 1 Deliverer,4dd 1 Zaos_Sword,2a 1 Ashmaker,4f 1 Rockville_Slugger,2e 1 Tinker_Tom_Special,1k 1 Old_Faithful,2k 1 Spray_N_Pray,2f 1 Justice,0b 1 PartyStarter,2m 1 Wastelanders_Friend,2c 1 Overseers_Guardian,1c 1 AX90_fury,1c 1 Experiment_18A,0a 1 Big_Boy,1k 1 Good_Intentions,4h 1 Kremvhs_Tooth,2f 1 Le_Fusil_Terribles,4i 1 Pickmans_Blade,1k 1 Righteous_Authority,1k 1 Wazer_Wifle,1c 1 Sentinels_Plasmacaster,0b 1 Death_from_Above,1e 1 Final_Judgment,2lb 1 Kelloggs_Pistol,2lb 1 The_Gainer,2lb 1 Eddies_Peace,4e 1 Grognaks_Axe,4f 1 2076_World_Series,1a 1 The_Last_Minute,4bd 1 Reckoning,4de 1 Shem_Drowne_Sword,4fb 1 Big_Jim,1k 1 Prototype_UP77,4dd 1 General_Chaos_Revenge,2c 1 Decembers_Child,0a 1 The_Striker,3a 1 Defenders_Harpoon_Gun,4fe 1 Bloodletter,4aa 1 Atoms_Judgement,2j 1 Radical_Conversion,2h 1 Lucky_Eddy,4f 1 Fencebuster,3a 1 Skippers_Last_Stand,4bd 1 The_Harvester,3a 1 Admirals_Friend,4jc 1 Butchers_Hook,4fe 1 The_Fish_Catcher,2j 1 Kiloton_Radium_Rifle,2h 1 Old_Reliable,1h 1 Sergant_Ash,2da 1 The_Problem_Solver,4i 1 Throatslicer,4dd 1 Sword_of_Wonders,2da 1 Splattercannon,4f 1 Citos_Shiny_Slugger,1e 1 Aeternus,1b 1 Hubs_Alien_Blaster,3k 1 Rexs_Prototype,2ka 1 Silver_Submachinegun,4ja 1 Swans_Power_Fist,PA Jetpack,Module,1 Ranged Weapons,2 Melee Weapons,3 Explosives,1 Outfits,2 Hats and Accessories,3 Armor and Power Armor,4 Dogs, Kids and Super Mutants Gear,1 Food,2 Drinks,3 Meds and Chems,4 Ingredients,1 Settings Holotapes,2 Collectibles,3 Valuables,4 Story Items,5 Recyclables';
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
        initTagifier();
        initTags();
    end;

    function Process(e: IInterface): integer;
    var
      newElem: IInterface;
    begin
        if Signature(e) = 'TES4' then
            Exit;

        if not Assigned(ToFile) then begin
            ToFile := showFileSelectionDialog(e);

            if not Assigned(ToFile) then begin
                Result := 1;
                Exit;
            end;
        end;

        // now actually do stuff
        newElem := tagifyElement(e, toFile);

    end;

    function Finalize: integer;
    begin
        Result := 0;
        cleanupTagifier();
    end;
end.