{
    Attempts to automatically figure out VIS tags for selected items and creates overrides for them with those tags.
    Additionally, text files can be put into Edit Scripts\tagging-overrides\ in the format of
        editor_id=[TAG]
    for manual renaming configuration

}
unit Fisifier;

    //uses VisOverrides;
    uses praFunctions;
    uses praTagifier;

    var
        ToFile: IInterface;

    procedure initTags();
    begin
        settingAddComponentString := false;

        tagShipment     := '[Scrap]';
        tagScrap        := '[Scrap]';
        tagResource     := '[Resource]';
        tagLooseMod     := '[Mod]';
        tagCollectible  := '[Collectible]';
        tagQuest        := '[Quest]';
        tagCurrency     := '[Currency]';
        tagValuable     := '[Valuable]';
        tagOtherMisc    := '[Other]';
        tagGoodChem     := '[Aid]';
        tagBadChem      := '[Chem]';
        tagFood         := '[Food]'; // generic food, selfcooked. Usually radless
        tagFoodRaw      := '[RadFood]'; // raw food, has rads, has disease risk
        tagFoodCrop     := '[Vegetables]'; // crops, has rads
        tagFoodPrewar   := '[RadFood]'; // prewar packaged, has rads
        tagDrink        := '[Drink]';
        tagLiquor       := '[Liquor]';
        tagNukacola     := '[Nuka]';
        tagSyringe      := '[SyringerDart]';
        tagDevice       := '[Aid]';
        tagTool         := '[Device]';
        tagNews         := '[NotePubOcc]';
        tagNote         := '[Note]';
        tagPerkmag      := '[PerkMag]';
        tagMine         := '[Mine]';
        tagGrenade      := '[Grenade]';
        tagKey          := '[Key]';
        tagAmmo         := '[Ammo]';
        tagHolotape     := '[Holotape]';
        tagHolotapeGame := '[Game]';
        tagHolotapeSettings := '[Settings]';
        tagPipBoy       := '[Pipboy]';
        // If any of the tags in this list, or any of the tags above are discovered (no matter the brackets), it will be considered "tagged", and skipped.
        extraValidTags  := 'Melee,Unarmed,MeleeOneHand,MeleeTwoHand,BoxingGlove,Gauntlet,PowerFist,Knuckles,BaseballBat,SuperSledge,TireIron,WalkingCane,PoolCue,RollingPin,LeadPipe,Baton,PipeWrench,Board,PaddleBall,CombatKnife,SwitchBlade,Shish,Sword,RevDword,Machete,Ripper,Axe,PoleHook,Ranged,Pistol,10mm,TheDeliverer,44P,InstitutePistol,PipeRevolver,LaserPistol,PlasmaPistol,AlienGun,FlareGun,ThirstZapper,AcidSoaker,Rifle,Cryo,Syringer,PipeRifle,Leveraction,InstituteRifle,HuntingRifle,Railway,GaussRifle,LaserRifle,PlasmaRifle,RadiumRifle,HarpoonGun,LaserMusket,HandmadeRifle,CombatRifle,AssaultRifle,Shotgun,DoubleShotgun,SMB,Minigun,Gatling,JunkJet,Cannon,Flamethrower,Rocketlauncher,Fatman,Ranged,Grenade,GrenadeCryo,GrenadePlasma,GrenadePulse,Molotov,Mine,MineCryo,MinePlasma,MinePulse,MineBottlecap,Trap,Ammo,AmmoCaliber,AmmoShells,AmmoEnergy,AmmoCannon,AmmoMissile,FusionCore,NonHuman,Unknown,SuperMutant,Clothes,Hat,Cap,Bandana,Eyes,Mask,GasMask,FullGasMask,Neck,Ring,Gloves,Underwear,FullOutfit,Dress,ProtectionSuit,Armor,FullArmor,VaultSuit,SetRaider,SetLeather,SetMetal,SetMarine,SetDCGuard,SetSynth,SetCombat,SetCustom,SetRobot,FullHelm,Helm,ArmL,ArmR,Torso,LegL,LegR,Jetpack,Holster,Pack,PowerArmor,PA_Raider,T45,T51,T60,X01,PowerHelm,PowerArmL,PowerArmR,PowerTorso,PowerLegL,PowerLegR,Food,RadFood,Soup,Stew,Steak,FoodStick,Cake,Boxed,Canned,Meat,Vegetables,Leaf,Shroom,Drink,Nuka,Liquor,Beer,Aid,Chem,Stimpak,MedPills,RadX,MedSyringe,MedSyringeGreen,MedSyringeOrange,MedInhalor,MedRobot,MedPackRed,RadAway,DrugPills,DrugPillsBlue,DrugPillsPurple,DrugPillsOrange,DrugSyringe,DrugSyringeRed,DrugSyringeOrange,DrugSyringePurple,DrugInhalor,StealthBoy,SyringerDart,Camping,PerkBobblehead,PerkMag,Collectible,Valuable,Note,NotePubOcc,NoteMisc,Key,Password,Passcard,Holotape,HolotapeT,HolotapeV,HolotapeP,Game,Mod,Settings,Device,Pipboy,Lockpick,Currency,Resource,Scrap,Bottle,Other,OtherALCH,Trash,Brotherhood,Minutemen,Institute,Railroad,VaultTec,Companion,Quest,MainQuest,DiamondCity,Goodneighbor,Cabot,Robot,FarHarbor,Acadia,Atom,Harbor,NukaWorld,Raid,Radio,SilverShroud,Warning,Military,Distress,Skylane,Fire,SkullCowboy,Atom2,Anarchy,Energy,StarOutline,Defense2,Biohazard,Water,Water2';
    end;

    function Initialize: integer;
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
                AddMessage('File selection dialog was cancelled or failed.');
                Result := 1;
                Exit;
            end;
        end;

        newElem := tagifyElement(e, toFile);

    end;
    
    function Finalize: integer;
    begin
        Result := 0;
        cleanupTagifier();
    end;

end.