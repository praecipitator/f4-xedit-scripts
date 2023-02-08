{
    Attempts to automatically figure out VIS tags for selected items and creates overrides for them with those tags.
    Additionally, test files can be put into Edit Scripts\vis-overrides\ in the format of
        editor_id=[VIS TAG] Name
        other_editor_id=Any Name whatsoever
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

        tagShipment     := '{Shipment}';
        tagScrap        := '{Scrap}';
        tagResource     := '{Resource}';
        tagLooseMod     := '{Mod}';
        tagCollectible  := '[Collectible]';
        tagQuest        := '[Quest]';
        tagCurrency     := '(Currency)';
        tagValuable     := '(Valuable)';
        tagOtherMisc    := '[Misc]'; // trash
        tagGoodChem     := '(Medicine)';
        tagBadChem      := '[Chem]';
        tagFood         := '(Food)'; // generic food, selfcooked. Usually radless
        tagFoodRaw      := '[Raw]'; // raw food, has rads, has disease risk
        tagFoodCrop     := '[Crop]'; // crops, has rads
        tagFoodPrewar   := '[Prewar]'; // prewar packaged, has rads
        tagDrink        := '(Drink)';
        tagLiquor       := '[Liquor]';
        tagNukacola     := '[Nuka]';
        tagSyringe      := '[Syringe]';
        tagDevice       := '(Device)';
        tagTool         := '(Tool)';
        tagNews         := '[News]';
        tagNote         := '[Note]';
        tagPerkmag      := '[PerkMag]';
        tagMine         := '(Mine)';
        tagGrenade      := '(Grenade)';
        tagKey          := '[Key]';
        tagAmmo         := '(Ammo)';
        tagHolotape     := '[Holotape]';
        tagHolotapeGame := '[Game]';
        tagHolotapeSettings := '[Settings]';
        tagPipBoy       := '[Pipboy]';
        extraValidTags  := 'Shipment,Scrap,Resource,Mod,Collectible,Quest,Currency,Valuable,Misc,Medicine,Chem,Food,Raw,Crop,Prewar,Drink,Nuka,Liquor,Syringe,Device,Tool,Note,Perk: Mag,News,Grenade,Mine,Trap,Holotape,Settings,Key,Ammo,Pipboy,Mutant,Dog,Game,Core,Book,Ore,Trash,Shroom,Herb,Ingredient,Bobblehead,Ring,Unique,Kremvh,Deliverer,Armor,Gun,Clothing,Melee,PArmor,ArmorChest,ArmorArm,ArmorLeg,ArmorHead,ArmorMask,Hat,Mask,Eyes,Flare,Pistol,ARifle,CRifle,RRifle,Rifle,Revolver,Syringer,Shotgun,Rocket,Railway,PRifle,PPistol,IRifle,IPistol,PipeR,Pipe,Lever,FatMan,Musket,LRifle,LPistol,JunkJet,Gauss,Gatling,Flamer,Cryolator,Cannon,Minigun,Gamma,Alien,SMG,Revolver,Harpoon,Shishkebab,Ripper,MeleeH2H,Melee1H,Melee2H,Bat,Sword,Knife,Sledge,PArmorLegR,PArmorLegL,PArmorArmR,PArmorArmL,PArmorChest,PArmorHead';
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