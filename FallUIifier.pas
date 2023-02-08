{
    Attempts to automatically figure out FallUI tags for selected items and creates overrides for them with those tags.
    Additionally, text files can be put into Edit Scripts\tagging-overrides\ in the format of
        editor_id=[TAG]
    for manual renaming configuration

}
unit Horizonifier;

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
        tagQuest        := '[Valuable]';
        tagCurrency     := '[Currency]';
        tagValuable     := '[Valuable]';
        tagOtherMisc    := '[Trash]'; // trash
        tagGoodChem     := '[Aid]';
        tagBadChem      := '[Chem]';
        tagFood         := '[Food]'; // generic food, selfcooked. Usually radless
        tagFoodRaw      := '[RadFood]'; // raw food, has rads, has disease risk
        tagFoodCrop     := '[RadFood]'; // crops, has rads
        tagFoodPrewar   := '[RadFood]'; // prewar packaged, has rads
        tagDrink        := '[Drink]';
        tagLiquor       := '[Liquor]';
        tagNukacola     := '[Drink]';
        tagSyringe      := '[Syringer]';
        tagDevice       := '[Device]';
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
        extraValidTags  := '10mm,44P,AlienGun,AmmoCaliber,AmmoEnergy,ArmL,ArmR,Armor,AssaultRifle,Beer,Bottle,Camping,Cannon,Clothes,CombatRifle,Cryo,Dress,Eyes,Fatman,Flamethrower,FullArmor,FullHelm,FullOutfit,FusionCore,Game,Gamma,GasMask,Gatling,GaussRifle,Gloves,HarpoonGun,Hat,Helm,HolotapeP,HolotapeT,HolotapeV,Holster,HuntingRifle,InstitutePistol,JunkJet,LaserPistol,LaserRifle,LegL,LegR,Leveraction,Lockpick,Mask,Minigun,Neck,NonHuman,NoteMisc,OtherALCH,Other,PA_Raider,Passcard,Password,PerkBobblehead,PipeRevolver,PipeRifle,Pistol,PlasmaPistol,PlasmaRifle,PowerArmor,RadiumRifle,Railway,Rifle,Ring,Rocketlauncher,SMB,SetCombat,SetCustom,SetDCGuard,SetLeather,SetMarine,SetMetal,SetRaider,SetSynth,Settings,Shotgun,SkillMag,StealthBoy,Stimpak,T45,T51,T60,TheDeliverer,Torso,Underwear,Unique,Unknown,VaultSuit,X01';
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
                AddMessage('File selection dialog was cancelled or failed.');
                Result := 1;
                Exit;
            end;
        end;
// AddMessage('WAT0');
        // now actually do stuff
        newElem := tagifyElement(e, toFile);

    end;
    
    function Finalize: integer;
    begin
        Result := 0;
        cleanupTagifier();
    end;

end.