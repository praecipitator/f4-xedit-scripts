{
    Attempts to automatically figure out VIS tags for selected items and creates overrides for them with those tags.
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
        settingAddComponentString := true;

        tagShipment     := '{Scrap}';
        tagScrap        := '{Scrap}';
        tagResource     := '{Resource}';
        tagLooseMod     := '{Mod}';
        tagCollectible  := '[Collectible]';
        tagQuest        := '[Quest]';
        tagCurrency     := '(Currency)';
        tagValuable     := '(Valuable)';
        tagOtherMisc    := '[Trash]'; // trash
        tagGoodChem     := '(Chem)';
        tagBadChem      := '[Chem]';
        tagFood         := '(Food)'; // generic food, selfcooked. Usually radless
        tagFoodRaw      := '[Raw]'; // raw food, has rads, has disease risk
        tagFoodCrop     := '[Raw]'; // crops, has rads
        tagFoodPrewar   := '[Prewar]'; // prewar packaged, has rads
        tagDrink        := '(Drink)';
        tagLiquor       := '[Liquor]';
        tagNukacola     := '[Nuka]';
        tagSyringe      := '[Syringe]';
        tagDevice       := '(Device)';
        tagTool         := '{Tool}';
        tagNews         := '[News]';
        tagNote         := '[Note]';
        tagPerkmag      := '[Perk: Mag]';
        tagMine         := '(Mine)';
        tagGrenade      := '(Grenade)';
        tagKey          := '|Key|';
        tagAmmo         := '(Ammo)';
        tagHolotape     := '[Holotape]';
        tagHolotapeGame := '[Game]';
        tagHolotapeSettings := '[Settings]';
        tagPipBoy       := '[Arm]';
        // If any of the tags in this list, or any of the tags above are discovered (no matter the brackets), it will be considered "tagged", and skipped.
        extraValidTags  := 'Aid,Device,Drink,Food,Chem,Liquor,Nuka,Prewar,Raw,Syringe,Ammo,Fuel,PA ArmL,PA ArmR,PA Torso,PA Helmet,PA LegL,PA LegR,ArmL,ArmR,Chest,LegL,LegR,Combat,Synth,DC Guard,Leather,Metal,Raider Power,T-45,T-51,T-60,X-01,Marine,Robot,Therm Optics,Trapper,Casual,Dapper,Institute,Military,Raider,Rugged,Skimpy,Vault-tec,Arm,Bandolier,Belt,Biosuit,Bottom,Bracelet,Cloak,Dog,Earring,Eyewear,Footwear,Gloves,Harness,Hat,Headband,Helmet,Jacket,Large Melee,Leg,Melee Weapon,Mouth,Mask,Necklace,Offhand,Pack,Piercing,Rifle,Ring,Satchel,Scarf,Sidearm,Super Mutant,Top,Vest,Armor,Clothing,Clothes,Underarmor,Explosives,Grenade,Mine,Signal,Resource,Scrap,Tool,Currency,Lockpick,Settings,Unique,Valuable,Bottle,Collectible,Crafting,Game,Hack,Holotape,Note,Password,Perk: Bobblehead,Perk: Mag,Quest,Trash,Key,Password,Passcard,Mod,Bobblehead,Magazine,Companion,.308,.38,.44,.45,.45-70,.50,2mm EC,5mm,5.56mm,10mm,Alien,Cannonball,Core,Cryo,Flamer,Flare,Fusion,Gamma,Junk,Melee,Mini Nuke,Missile,Plasma,Spike,Shotgun,Syringer,Harpoon,7.62,Acid,String,Zapper,PA Jetpack,Module';
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