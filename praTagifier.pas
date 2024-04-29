{
    Functions to tag items
}
unit praTagifier;

    //uses VisOverrides;
    uses praUtil;
    uses praFunctions;
    
    const
        tagExtractionRegex = '^([\[\]\(\)\{\}|][^\[\]\(\)\{\}|]+[\[\]\(\)\{\}|]).+';

    var
        scriptBlackList: TStringList;
        scriptBlackListPrefix: TStringList;
        edidBlackListPrefix: TStringList;
        sigsToProcess: TStringList;
        weaponTypesMelee: TStringList;
        extraValidTagsList: TStringList;
        gameProgramList: TStringList;
        
        fileToClean: IInterface;

        // TAGS
        tagShipment: String; // Shipments
        tagShipmentValuable: String; // Shipments, but fallback to valuable or collectible or such
        tagScrap: String; // Scrap, MISCs which contain components
        tagResource: String; // Resources, MISCs which a meant to represent one type of component
        tagLooseMod: String; // Loose modifications
        tagCollectible: String; // "Collectible" MISCs
        tagQuest: String; // Quest items
        tagCurrency: String; // "Currency", MISCs with zero weight and non-zero value
        tagValuable: String; // MISCs with more value than weight
        tagOtherMisc: String;// all other MISCs, trash
        tagGoodChem: String; // Cures etc
        tagBadChem: String; // Addictive chems
        tagFood: String; // generic food, selfcooked. Usually radless
        tagFoodRaw: String; // raw food, has rads, has disease risk
        tagFoodCrop: String; // crops, has rads
        tagFoodPrewar: String; // prewar packaged, has rads
        tagDrink: String; // generic drinkable
        tagLiquor: String;// Alcoholic beverages
        tagNukacola: String; // Nuka Cola of any kind
        tagSyringe: String; // Syringer ammo
        tagDevice: String;// Consumables which are supposed to be devices instead of something to eat, like the Stealth Boy
        tagTool: String; // Similar to above, but for more low-tech things. Like SimSettlements Town Meeting Gavel, or the Companion Whistle
        tagNews: String; // Newspaper
        tagNote: String; // Any paper note
        tagPerkmag: String; // Perk Magazine
        tagMine: String; // Mine
        tagGrenade: String; // Grenade
        tagKey: String;// Keycard
        tagAmmo: String; // Generic Ammo
        tagHolotape: String; // Holotape
        tagHolotapeGame: String; // Game Holotape
        tagHolotapeSettings: String; // Settings Holotape
        tagPipBoy: String; // player's pip-boy, also the MISC item from Vault88
        extraValidTags: String; // comma-separated list of additional tags which should be sonsidered valid

        settingAddComponentString: boolean;

        innrCommonMelee: IInterface;
        innrCommonArmor: IInterface;
        innrPowerArmor: IInterface;
        innrClothes: IInterface;
        innrCommonGun: IInterface;
        innrVaultSuit: IInterface;
        
    function prefixNameWithTag(tag: string; name: string): string;
    var
        nameCopy, cleanName: String;
        char: String;
        i: integer;
    begin
        nameCopy := name;
        cleanName := name;
        if(nameCopy = '') then begin
            raise Exception.Create('prefixNameWithTag called with an empty name! This is not valid!');
        end;
        if(tag = '') then begin
            raise Exception.Create('prefixNameWithTag called with an empty tag! This is not valid!');
        end;
        for i := 1 to length(nameCopy) do begin
            char := nameCopy[i];
            if not ((char = ' ') or (char = '-')) then begin
                cleanName := copy(nameCopy, i, length(nameCopy));

                break;
            end;
        end;

        Result := tag + ' ' + cleanName;

    end;
        
    function getTagContent(tag: string): string;
    begin
        if(length(tag) < 2) then begin
            Result := '';
            exit;
        end;
        Result := copy(tag, 2, length(tag)-2);
    end;

    procedure initExtraValidTagsList();
    var
        i: integer;
        helper: TStringList;
    begin
        extraValidTagsList := TStringList.create;
        
        extraValidTagsList.Sorted := True;
        extraValidTagsList.Duplicates := dupIgnore;

        extraValidTagsList.add(getTagContent(tagShipmentValuable));
        extraValidTagsList.add(getTagContent(tagShipment));
        extraValidTagsList.add(getTagContent(tagScrap));
        extraValidTagsList.add(getTagContent(tagResource));
        extraValidTagsList.add(getTagContent(tagLooseMod));
        extraValidTagsList.add(getTagContent(tagCollectible));
        extraValidTagsList.add(getTagContent(tagQuest));
        extraValidTagsList.add(getTagContent(tagCurrency));
        extraValidTagsList.add(getTagContent(tagValuable));
        extraValidTagsList.add(getTagContent(tagOtherMisc));
        extraValidTagsList.add(getTagContent(tagGoodChem));
        extraValidTagsList.add(getTagContent(tagBadChem));
        extraValidTagsList.add(getTagContent(tagFood));
        extraValidTagsList.add(getTagContent(tagFoodRaw));
        extraValidTagsList.add(getTagContent(tagFoodCrop));
        extraValidTagsList.add(getTagContent(tagFoodPrewar));
        extraValidTagsList.add(getTagContent(tagDrink));
        extraValidTagsList.add(getTagContent(tagLiquor));
        extraValidTagsList.add(getTagContent(tagNukacola));
        extraValidTagsList.add(getTagContent(tagSyringe));
        extraValidTagsList.add(getTagContent(tagDevice));
        extraValidTagsList.add(getTagContent(tagTool));
        extraValidTagsList.add(getTagContent(tagNews));
        extraValidTagsList.add(getTagContent(tagNote));
        extraValidTagsList.add(getTagContent(tagPerkmag));
        extraValidTagsList.add(getTagContent(tagMine));
        extraValidTagsList.add(getTagContent(tagGrenade));
        extraValidTagsList.add(getTagContent(tagKey));
        extraValidTagsList.add(getTagContent(tagAmmo));
        extraValidTagsList.add(getTagContent(tagHolotape));
        extraValidTagsList.add(getTagContent(tagHolotapeGame));
        extraValidTagsList.add(getTagContent(tagHolotapeSettings));
        extraValidTagsList.add(getTagContent(tagPipBoy));
        
        if(extraValidTags <> '') then begin
            helper := TStringList.create();
            helper.Delimiter := ',';
            helper.StrictDelimiter := True; // Spaces excluded from being a delimiter
            helper.DelimitedText := extraValidTags;
            
            for i := 0 to helper.count-1 do begin
                extraValidTagsList.add(helper[i]);
            end;
            
            helper.free();
        end;
    end;

    function isTagValid(tag: string): boolean;
    var
        tagNoBrackets: string;
    begin
        if(extraValidTagsList = nil) then begin
            initExtraValidTagsList();
        end;
        
        tagNoBrackets := getTagContent(tag);
        
        Result := extraValidTagsList.indexOf(tagNoBrackets) > -1;
    end;


    {
        return true if this has a valid tag already
    }
    function checkItemTag(text: String): boolean;
    var
        c, tag, tagNoBrackets: string;
        i, len: integer;
    begin
        Result := false;
        len := length(text);
        if(len = 0) then begin
            exit;
        end;
        // regex := '^([\[\]\(\)\{\}|][^\[\]\(\)\{\}|]+[\[\]\(\)\{\}|]).+';
        tag := regexExtract(text, tagExtractionRegex, 1);
        if(tag = '') then begin
            exit;
        end;
        
        // for where the entire name is in brackets
        if(tag = text) then begin
            exit;
        end;
        
        Result := isTagValid(tag);
    end;
    // function initTagifier

    // seems like I can't return arrays in this PoS
    function getComponents(e: IInterface): String;
    var
        numComponents: integer;
        j: integer;
        componentMain: IInterface;
        componentRoot: IInterface;
        component: IInterface;
        componentValue: IInterface;
        quantity: integer;
    begin
        Result := '';
        componentRoot := ElementByName(e, 'CVPA - Components');
        if assigned(componentRoot) then begin
            numComponents := ElementCount(componentRoot);

            if numComponents > 0 then begin
            
                for j := 0 to numComponents-1 do begin
                    componentMain := ElementByIndex(componentRoot, j);

                    component := ElementByName(componentMain, 'Component');
                    if assigned(component) then begin
                        component := LinksTo(component);

                        componentValue := ElementByName(componentMain, 'Count');

                        quantity := StrToInt(GetEditValue(componentValue));
                        if quantity > 0 then begin
                            if Result <> '' then begin
                                Result := Result + ', ';
                            end
                            Result := Result + GetElementEditValues(component, 'FULL');
                        end
                    end;
                end;
            end;
        end;
    end;

    function getComponentsList(e: IInterface): TStringList;
    var
        numComponents: integer;
        j: integer;
        componentMain: IInterface;
        componentRoot: IInterface;
        component: IInterface;
        componentValue: IInterface;
        quantity: integer;
    begin
        Result := TStringList.create;
        componentRoot := ElementByName(e, 'CVPA - Components');
        if assigned(componentRoot) then begin
            numComponents := ElementCount(componentRoot);

            if numComponents > 0 then begin
                for j := 0 to numComponents-1 do begin
                    componentMain := ElementByIndex(componentRoot, j);

                    component := ElementByName(componentMain, 'Component');
                    if assigned(component) then begin
                        component := LinksTo(component);

                        componentValue := ElementByName(componentMain, 'Count');

                        quantity := StrToInt(GetEditValue(componentValue));
                        if quantity > 0 then begin
                            Result.add(EditorID(component));
                        end
                    end;

                end;
            end;
        end;

    end;

    function hasKWDA(e: IInterface; kw: String): boolean;
    var
        kwda: IInterface;
        curKW: IInterface;
        i: Integer;
    begin
        Result := hasKeywordBySignature(e, kw, 'KWDA');
    end;

    function hasScript(e: IInterface; kw: String): boolean;
    begin
        Result := hasScriptMulti(e, kw, false);
    end;

    function hasScriptStartingWith(e: IInterface; kw: String): boolean;
    begin
        Result := hasScriptMulti(e, kw, true);
    end;

    function removeComponentString(text: String): String;
    var
        i: integer;
        ending: integer;
        len: integer;
        c: String;
    begin
        len := length(text);
        Result := text;

        if(len < 3) then begin
            exit;
        end;

        if (text[len] = '}') and (text[len-1] = '}') and (text[len-2] = '}') then begin
            // find the {{{

            for i := len-3 downto 3 do begin

                if (text[i] = '{') and (text[i-1] = '{') and (text[i-2] = '{') then begin
                    // found

                    Result := copy(text, 0, i-3);
                    exit;
                end;
            end;

        end;

    end;

    function getModel(e: IInterface): string;
    begin
        Result := GetElementEditValues(e, 'Model\MODL');
    end;

    function isLooseMod(e: IInterface): boolean;
    var
        i, numRefs: integer;
        curRef, curLnam: IInterface;
        curSig: string;
    begin
        Result := false;
        if hasKWDA(e, 'ObjectTypeLooseMod') then begin
            Result := true;
            exit;
        end;

        // now check usages
        numRefs := ReferencedByCount(e)-1;
        for i := 0 to numRefs do begin
            curRef := ReferencedByIndex(e, i);
            curSig := Signature(curRef);
            if(curSig = 'OMOD') then begin
                curLnam := pathLinksTo(curRef, 'LNAM');
                if(isSameForm(curLnam, e)) then begin
                    Result := true;
                    exit;
                end;
            end;
        end;
    end;

    function hasAnyScriptInPrefixList(e: IInterface; list: TStringList): boolean;
    var
        j, i: integer;
        curScript, vmad, curWat: IInterface;
        curScriptName: string;
    begin
        vmad := ElementByName(e, 'VMAD - Virtual Machine Adapter');
        Result := false;

        for i := 0 to ElementCount(vmad)-1 do begin
            curScript := ElementByIndex(vmad, i);
            for j := 0 to ElementCount(curScript)-1 do begin
                curWat := ElementByIndex(curScript, j);
                curScriptName := GetElementEditValues(curWat, 'scriptName');

                if(hasPrefixInList(curScriptName, list)) then begin
                    Result := true;
                    exit;
                end;
            end;
        end;
    end;
    
    function processHolotape(e: IInterface): string;
    var
        holotapeType, curName, tagToUse, programName, edid: string;
    begin

        curName := DisplayName(e);
        holotapeType := getElementEditValues(e, 'DNAM');
        if(holotapeType = 'Sound') or (holotapeType = 'Voice') then begin
            Result := prefixNameWithTag(tagHolotape, curName);
            exit;
        end;
        
        if (holotapeType = 'Program') then begin
            programName := getElementEditValues(e, 'PNAM');
            if(programName <> '') then begin
                programName := LowerCase(programName);
                if(gameProgramList.indexOf(programName) >= 0) then begin
                    Result := prefixNameWithTag(tagHolotapeGame, curName);
                    exit;
                end;
            end;
        end;

        
        // Result := strStartsWithCI(fullString, list[index-1]);
        // special
        if(strStartsWith(curName, '- ')) then begin
            Result := prefixNameWithTag(tagHolotapeSettings, curName);
            exit;
        end;
        
        if (pos('setting', curName) > 0 or pos('config', curName) > 0) then begin
            Result := prefixNameWithTag(tagHolotapeSettings, curName);
            exit;
        end;
        
        edid := LowerCase(EditorID(e));
        
        if (pos('setting', edid) > 0 or pos('config', edid) > 0 or pos('cheat', edid) > 0) then begin
            Result := prefixNameWithTag(tagHolotapeSettings, curName);
            exit;
        end;
        
        Result := prefixNameWithTag(tagHolotape, curName);
    end;

    function processMisc(e: IInterface; components: String): String;
    var
        value: integer;
        weight: float;
        numComponents: integer;
        i: integer;
        entry: IInterface;
        componentcontainer: IInterface;
        curName: String;
        //components: String;
    begin
        // AddMessage('Processing misc');
        Result := '';
        curName := DisplayName(e);

        // test pipboy
        if(hasScript(e, 'DLC06:PipboyMiscItemScript')) or (hasScript(e, 'CreationsByCOOTS:PipboyMiscItemScript')) then begin
            Result := prefixNameWithTag(tagPipBoy, curName);
            exit;
        end;
        
        if(hasScript(e, 'praVRF:SimulationData')) then begin
            Result := prefixNameWithTag(tagHolotape, curName);
            exit;
        end;

        // it can be {Scrap}
        weight := StrToFloat( GetElementEditValues(e, 'DATA\Weight') );
        value := StrToInt( GetElementEditValues(e, 'DATA\Value') );

        //components := getComponents(e);

        if components <> '' then begin
            // hack for gears, mostly
            if (curName = components) or (curName = components+'s') then begin
                // resource
                Result := prefixNameWithTag(tagResource, curName);
                exit;
            end;
            // scrap
            // exception for shipments
            // ShipmentScript
            if hasScript(e, 'ShipmentScript') then begin
                Result := prefixNameWithTag(tagShipment, curName);
                exit;
            end;

            if(settingAddComponentString) then begin
                Result := prefixNameWithTag(tagScrap, removeComponentString(curName)+'{{{'+components+'}}}');
            end else begin
                Result := prefixNameWithTag(tagScrap, removeComponentString(curName));
            end;
            exit;

        end;

        if isLooseMod(e) then begin
            Result := prefixNameWithTag(tagLooseMod, curName);
            //Result := '[Mod] '+curName;
            exit;
        end;
        
        if hasKWDA(e, 'FeaturedItem') then begin
            Result := prefixNameWithTag(tagCollectible, curName);
            //Result := '[Collectible] '+curName;
            exit;
        end;

        if hasKWDA(e, 'VendorItemNoSale') or hasKWDA(e, 'UnscrappableObject') then begin
            Result := prefixNameWithTag(tagQuest, curName);
            //Result := '[Quest] '+curName;
            exit;
        end;

        if(LowerCase(getModel(e)) = 'props\pipboymiscitem\pipboymisc01.nif') then begin
            Result := prefixNameWithTag(tagPipBoy, curName);
            exit;
        end;


        // START
        if ((weight = 0) and (value > 0)) then begin
            // weightless but valuable -> currency
            Result := prefixNameWithTag(tagCurrency, curName);
        end else if (value > 0) and (value >= weight) then begin
            // has weight and valuable -> valuable
            Result := prefixNameWithTag(tagValuable, curName);
        end else begin
            // otherwise trash
            Result := prefixNameWithTag(tagOtherMisc, curName);
        end;
        // END


    end;


    function processAlch(e: IInterface): String;
    var
        alchType: integer;
        tag: String;
        curName: String;
    begin
        //curName := checkVisTag('foo');
        curName := DisplayName(e);
        alchType := getAlchemyType(e);


        tag := tagGoodChem;
        case (alchType) of
            1:  tag := tagGoodChem;
            2:  tag := tagBadChem;
            10: tag := tagFood; // generic food, cooked
            11: tag := tagFoodRaw; // raw
            12: tag := tagFoodPrewar; // prewar
            13: tag := tagFoodCrop; // crop
            20: tag := tagDrink; // purified water uses [==]
            21: tag := tagLiquor; // and {} here
            22: tag := tagNukacola; // C for Cola?
            30: tag := tagSyringe; // is ok
            40: tag := tagDevice; //(^) seem to be devices
            41: tag := tagTool; //tools
        end;
        //Result := tag + ' ' + curName;
        Result := prefixNameWithTag(tag, curName);
    end;

    function checkInnr(e: IInterface): boolean;
    begin
        Result := false;

        if(not assigned(e)) then begin
            exit;
        end;
        // for now, return true if assigned
        Result := true;
         {
        // if e is one of the default things, return true
        if(
            isSameForm(e, innrCommonMelee) or
            isSameForm(e, innrCommonArmor) or
            isSameForm(e, innrPowerArmor) or
            isSameForm(e, innrClothes) or
            isSameForm(e, innrCommonGun) or
            isSameForm(e, innrVaultSuit)

        ) then begin
            Result := true;
            exit;
        end;
        }
        // TODO other stuff

    end;

    function hasNonPlayableFlag(e: IInterface): boolean;
    var
        flags: IInterface;
        strVal: string;
    begin
        flags := ElementByNamePath(e, 'Record Header\Record Flags');

        strVal := GetElementEditValues(flags, 'Non-Playable');

        Result := (strVal = '1');

    end;

    procedure ensureDefaultObjectTemplate(e: IInterface);
    var
        template, combos, curCombo: IInterface;
        numCombos: integer;
    begin
        // ensure a default template exists. It seems that armor needs this to exist. Maybe weapons do, too?
        template := ElementByPath(e, 'Object Template');
        if(not assigned(template)) then begin
            // add it. this does NOT add a combination yet
            template := Add(e, 'Object Template', true);
        end;

        combos := ElementByPath(template, 'Combinations');

        if(not assigned(combos)) then begin
            // Combinations cannot be added
            // found this via trial&error
            combos := ElementAssign(template, 1, nil, false);
            curCombo := ElementByIndex(combos, 0);

            // default := true and Addon INdex := -1
            SetElementEditValues(curCombo, 'OBTS\Addon Index', '-1');
            SetElementEditValues(curCombo, 'OBTS\Default', 'true');
            exit;
        end;


        {
        numCombos := IntToStr(GetElementEditValues(template, 'OBTE'));
        if(numCombos <= 0) then begin
            // add a combo
        end;
        }
        {
        Object Template=
  OBTE - Count=1
  Combinations=
    Combination #0=
      OBTF - Editor Only=
      FULL - Name=Default
      OBTS - Object Mod Template Item=
        Include Count=0
        Property Count=0
        Level Min=0
        Unused=00
        Level Max=0
        Unused=00
        Addon Index=-1
        Default=True
        Keywords=
        Min Level For Ranks=0
        Alt Levels Per Tier=0
        Includes=
        Properties=
  STOP - Marker=
  }
    end;

    procedure processArmo(e: IInterface);
    var
        armorRating: integer;
        armorRatingStr: String;
        isNotPlayable: boolean;
        armoInrd: IInterface;
        armoType: integer; // 0 = cloth, 1 = armor, 2 = power armor, 3 = vault suit, 4 = pipboy
        newOverride: IInterface;
        newInrd: IInterface;
        newInnr: IInterface;
        bod2tags: String;
        bod2flags: cardinal;
    begin

        armoInrd := LinksTo(ElementBySignature(e, 'INRD'));

        if(checkInnr(armoInrd)) then begin
            // nothing to do
            exit;
        end;

        armoType := -1;
        bod2flags := 0;

        // check pipboy
        bod2tags := GetElementEditValues(e, 'BOD2\First Person Flags');
        if(bod2tags <> '') then begin
            // it seems that these flags are in the wrong order...
            bod2tags := StringReverse(bod2tags);
            bod2flags := BinToInt(bod2tags);
            //AddMessage('Flags: '+bod2tags+' '+IntToStr(bod2tags));
            if(bod2flags = $40000000) then begin
                // only the pip-boy flag is set
                // 0001110000000000000000000000001 -> first 3 are BODY, LHand, RHand
                armoType := 4;
            end;
        end;
        

        if(armoType < 0) then begin

            if(hasNonPlayableFlag(e)) then begin
                exit;
            end;

            if(hasKeywordBySignature(e, 'VaultSuitKeyword', 'KWDA')) then begin
                armoType := 3;
            end else if(hasKeywordBySignature(e, 'ArmorTypePower', 'KWDA')) then begin
                armoType := 2;
            end else begin
                armorRating := 0;
                armorRatingStr := GetElementEditValues(e, 'FNAM\Armor Rating');
                if armorRatingStr <> '' then begin
                    armorRating := StrToInt(armorRatingStr);
                end;
                if armorRating = 0 then begin
                    armoType := 0;
                end else begin
                    armoType := 1;
                end;
            end;
        end;

        if(armoType < 0) then begin
            exit;
        end;

        // if we are still here, we need an override.
        newOverride := getOrCreateElementOverride(e, ToFile);
        if(armoType = 4) then begin
            //AddMessage('WAT '+GetElementEditValues(e, 'FULL'));
            //AddMessage('FU '+tagPipBoy);
            SetElementEditValues(newOverride, 'FULL', prefixNameWithTag(tagPipBoy, GetElementEditValues(e, 'FULL')));
            exit;
        end;


        if(armoType = 1) then begin
            newInnr := innrCommonArmor;
        end else if(armoType = 2) then begin
            newInnr := innrPowerArmor;
        end else begin
            newInnr := innrClothes;
        end;

        AddMessage('Adding INNR to '+DisplayName(e));
        newInrd := Add(newOverride, 'INRD', True);
        SetEditValue(newInrd, IntToHex(GetLoadOrderFormID(newInnr), 8));

        ensureDefaultObjectTemplate(newOverride);
    end;

    function processNote(e: IInterface): String;
    var
        curName, curModel: String;
    begin
        curName := DisplayName(e);

        if (hasScript(e, 'SimSettlements:Newspaper')) then begin
        //SimSettlements:Newspaper
            Result := prefixNameWithTag(tagNews, curName);
            //Result := '[#Note] '+curName; // no separate tag for news?
            exit;
        end;
        
        if (hasScript(e, 'SimSettlementsV2:Books:NewsArticle')) then begin
        //SimSettlements:Newspaper
            Result := prefixNameWithTag(tagNews, curName);
            //Result := '[#Note] '+curName; // no separate tag for news?
            exit;
        end;

        curModel := getModel(e);
        if(curModel = 'Props\NewspaperPublickOccurencesLowPoly.nif') then begin
            Result := prefixNameWithTag(tagNews, curName);
            exit;
        end;

        // script: CA_SkillMagazineScript
        if (hasKWDA(e, 'PerkMagKeyword') or hasScript(e, 'CA_SkillMagazineScript')) then begin
            Result := prefixNameWithTag(tagPerkmag, curName);
            //Result := '[Magazine] '+curName;
            exit;
        end;

        Result := prefixNameWithTag(tagNote, curName);
        //Result := '[#Note] '+curName; // there is also holotape?


        //PerkMagKeyword
    end;

    function getUsedAmmo(e: IInterface): IInterface;
    var
        data: IInterface;
    begin
        Result := nil;
        data := ElementBySignature(e, 'DNAM');
        if data <> nil then begin
            Result := LinksTo(ElementByName(data, 'Ammo'));
        end;
    end;



    function processWeap(e: IInterface): String;
    var
        curName: String;
        ammo: IInterface;
        weapInrd: IInterface;
        newOverride: IInterface;
        flags: IInterface;
    begin
        if(hasNonPlayableFlag(e)) then begin
            exit;
        end;

        curName := DisplayName(e);

        // now only process explosives
        if hasKWDA(e, 'WeaponTypeExplosive') then begin
            // check if this has ammo
            ammo := getUsedAmmo(e);
            if (not assigned(ammo)) then begin
                // some explosive
                if hasKWDA(e, 'AnimsMine') then begin
                    Result := prefixNameWithTag(tagMine, curName);
                end else begin
                    Result := prefixNameWithTag(tagGrenade, curName);
                end;
            end;
            exit;
        end;

        if(hasKWDA(e, 'WeaponTypeGrenade')) then begin
            Result := prefixNameWithTag(tagGrenade, curName);
            exit;
        end;

        if(hasKWDA(e, 'WeaponTypeMine')) then begin
            Result := prefixNameWithTag(tagMine, curName);
            exit;
        end;

        // maybe still some thing


        // otherwise, this is a normal weapon
        weapInrd := LinksTo(ElementBySignature(e, 'INRD'));

        if(checkInnr(weapInrd)) then begin
            // nothing to do
            exit;
        end;

        newOverride := getOrCreateElementOverride(e, ToFile);

        AddMessage('Adding INNR to '+DisplayName(e));
        if(hasAnyKeyword(e, weaponTypesMelee, 'KWDA')) then begin
            // melee
            SetEditValue(Add(newOverride, 'INRD', True), IntToHex(GetLoadOrderFormID(innrCommonMelee), 8));
        end else begin
            SetEditValue(Add(newOverride, 'INRD', True), IntToHex(GetLoadOrderFormID(innrCommonGun), 8));
        end;

        ensureDefaultObjectTemplate(newOverride);
    end;

    {
        Why did I call these things "overrides"? It confuses the shit out of me now...
    }
    function GetOverrideForName(e: IInterface; key: String; oldName: String; components: String) : String;
    var
        prefix: String;
    begin
        Result := '';
        prefix := GetOverride(key);

        if (oldName <> '') and (prefix <> '') then begin
            // if(components != '' && !(prefix == 'Component && components == oldName))
            // !(prefix == 'Component && components == oldName) -> (prefix != 'Component || components != oldName)
            // -> (components != '' && (prefix != 'Component || components != oldName))

            if (components <> '') and ((prefix <> '[Component]') or (components <> oldName)) and (not hasScript(e, 'ShipmentScript')) then begin

                if(settingAddComponentString) then begin
                    Result := prefixNameWithTag(prefix, removeComponentString(oldName) + '{{{' + components + '}}}');
                end else begin
                    Result := prefixNameWithTag(prefix, removeComponentString(oldName));
                end;
                //Result := prefix + ' ' + oldName + '{{{' + components + '}}}';
            end else begin
                Result := prefixNameWithTag(prefix, oldName);
                //Result := prefix + ' ' + oldName;
            end;
        end;
    end;

    function getNameFromExistingOverride(winOverride: IInterface; key: String; oldName: String; components: String): string;
    var
        numOverrides, i: integer;
        curOverride, masterElem: IInterface;
        curName, oldName, curTag: string;
        curFile, winOverrideFile: IInterface;
    begin
        masterElem := MasterOrSelf(winOverride);
        Result := '';
        numOverrides := OverrideCount(masterElem);
        if(numOverrides = 0) then exit;

        winOverrideFile := GetFile(winOverride);
        oldName := DisplayName(winOverride);

        for i:=numOverrides-1 downto 0 do begin
            curOverride := OverrideByIndex(masterElem, i);
            curFile := GetFile(curOverride);

            if(FilesEqual(winOverride, curOverride)) then begin
                exit;
            end;

            curName := DisplayName(curOverride);
            curTag := extractVisTag(curName);

            if (curTag <> '') then begin
                AddMessage('Found tag '+curTag+' in element''s overrides');

                if (components <> '') and ((curTag <> '[Component]') or (components <> oldName)) and (not hasScript(curOverride, 'ShipmentScript')) then begin
                    if(settingAddComponentString) then begin
                        Result := prefixNameWithTag(curTag, removeComponentString(oldName) + '{{{' + components + '}}}');
                    end else begin
                        Result := prefixNameWithTag(curTag, removeComponentString(oldName));
                    end;
                end else begin
                    Result := prefixNameWithTag(curTag, oldName);
                end;
                exit;
            end;
        end;
    end;

    function hasPrefixInList(fullString: string; list: TStringList): boolean;
    var
        index, prevIndex: integer;
    begin
        Result := false;
        if(list.count = 0) then exit;

        if(list.find(fullString, index)) then begin
            Result := true;
            exit;
        end;

        // if prefix exists, this entry would have been put right after it
        if(index < 1) then exit;

        Result := strStartsWithCI(fullString, list[index-1]);
//        AddMessage('Compared: '+list[index-1]+' vs '+fullString+' -> '+BoolToStr(Result));
    end;

    procedure initTagifier();
    var
        dir: String;
        Path: String;
        pattern: String;
        Attr: Integer;
        FileAttr: Integer;
        Res: TSearchRec;
        Name: String;

        foo: string;
    begin
        extraValidTagsList := nil;
        extraValidTags := '';
        dir := ScriptsPath + 'tagging-overrides\';
        pattern := '*.txt';
        Attr := faAnyFile;

        prepareOverrides();


        if FindFirst(dir+Pattern, Attr, Res) = 0 then
        begin
          Path := ExtractFileDir(dir+Pattern);

          repeat
            foo := Res.Name;
             Name := Path+'\'+Res.Name;

             FileAttr := FileGetAttr(Name);
             if FileAttr and faDirectory = 0 then
             begin
                { Do something with file name }
                AddMessage('Loading overrides from '+Name);
                loadOverrides(Name);
             end;
          until FindNext(Res) <> 0;
        end;
        FindClose(Res);
        AddMessage('All overrides loaded');
        //DictionaryFile := ScriptsPath + 'vis-overrides\';

        scriptBlackList := TStringList.create;
        scriptBlackList.Sorted := true;
        scriptBlackList.CaseSensitive := false;


        scriptBlackList.add('SimSettlements:SimBuildingPlan');
        scriptBlackList.add('SimSettlements:SimStory');
        scriptBlackList.add('SimSettlements:CityPlan');
        scriptBlackList.add('SimSettlements:SimPlanPath');
        scriptBlackList.add('SimSettlements:DynamicFlag');
        scriptBlackList.add('SimSettlements:LeaderCard');
        scriptBlackList.add('SimSettlements:medicalresearchrecipe');
        scriptBlackList.add('SimSettlements:CityPlanLayer');
        scriptBlackList.add('SimSettlements:SimBuildingPlanSkin');
        scriptBlackList.add('SimSettlements:SpawnableFoundation');
        scriptBlackList.add('SimSettlements:FactionUnitData');
        scriptBlackList.add('WorkshopPlus:ObjectReferences:Blueprint');

        // SS2

        // MISCs
        scriptBlackList.add('SimSettlementsV2:MiscObjects:AddonPackConfiguration');
        scriptBlackList.add('SimSettlementsV2:MiscObjects:BuildingPlanTheme');
        scriptBlackList.add('SimSettlementsV2:MiscObjects:CameraControlSequence');
        scriptBlackList.add('SimSettlementsV2:MiscObjects:CharacterStatCard');
        scriptBlackList.add('SimSettlementsV2:MiscObjects:Foundation');
        scriptBlackList.add('SimSettlementsV2:MiscObjects:PowerPole');

        // MAYBE SimSettlementsV2:MiscObjects:FurnitureStoreItem
        // MAYBE SimSettlementsV2:MiscObjects:PetStoreCreatureItem
        // MAYBE

        scriptBlackList.add('SimSettlementsV2:MiscObjects:NPCPreferences');
        scriptBlackList.add('SimSettlementsV2:MiscObjects:OptionsProfile');

        scriptBlackList.add('SimSettlementsV2:MiscObjects:PlotMessages');
        scriptBlackList.add('SimSettlementsV2:MiscObjects:SettlerLocationDiscovery');
        scriptBlackList.add('SimSettlementsV2:MiscObjects:SoundscapeLayer');
        scriptBlackList.add('SimSettlementsV2:MiscObjects:StageItem');

        scriptBlackList.add('SimSettlementsV2:MiscObjects:UsageRequirements');
        scriptBlackList.add('SimSettlementsV2:MiscObjects:MedicalResearchRecipe');
        scriptBlackList.add('WorkshopFramework:Library:ObjectRefs:PreventLooting');


        scriptBlackListPrefix := TStringList.create;
        scriptBlackListPrefix.sorted := true;
        scriptBlackListPrefix.CaseSensitive := false;
        scriptBlackListPrefix.add('AutoBuilder:');
        scriptBlackListPrefix.add('SimSettlementsV2:Armors:');
        scriptBlackListPrefix.add('SimSettlementsV2:Weapons:');
        scriptBlackListPrefix.add('SimSettlementsV2:HQ:');
        scriptBlackListPrefix.add('SimSettlementsV2:MiscObjects:LeaderTrait');
        scriptBlackListPrefix.add('SimSettlementsV2:MiscObjects:ThemeRuleSet');
        scriptBlackListPrefix.add('SimSettlementsV2:MiscObjects:PlotConfiguration');
        scriptBlackListPrefix.add('SimSettlementsV2:MiscObjects:Unlockable');

        edidBlackListPrefix := TStringList.create;
        edidBlackListPrefix.sorted := true;
        edidBlackListPrefix.CaseSensitive := false;
        edidBlackListPrefix.add('SS2_NameHolder_');
        edidBlackListPrefix.add('SS2_Unlockable_');
        edidBlackListPrefix.add('SS2_LeaderTrait_');
        edidBlackListPrefix.add('SS2_SLCP_');
        edidBlackListPrefix.add('SS2_Skin_');
        edidBlackListPrefix.add('SS2_BP_Randomizer');
        edidBlackListPrefix.add('HC_Cannibal_RavenousHunger');
        edidBlackListPrefix.add('HC_DiseaseEffect_');
        edidBlackListPrefix.add('HC_Effect_');
        edidBlackListPrefix.add('HC_EncumbranceEffect_');
        edidBlackListPrefix.add('HC_AdrenalineEffect');
        edidBlackListPrefix.add('HC_HungerEffect_');
        edidBlackListPrefix.add('HC_SleepEffect_');
        edidBlackListPrefix.add('HC_ThirstEffect_');
        // edidBlackListPrefix.add('ArmorPoweredFrame');
        edidBlackListPrefix.add('WSFW_NameHolder_');
        edidBlackListPrefix.add('kgConq_AssaultQuestVerb_');
        edidBlackListPrefix.add('RECYCLED_MISC_');
        edidBlackListPrefix.add('WSFW_Blank');




        sigsToProcess := TStringList.create;

        sigsToProcess.add('MISC');
        sigsToProcess.add('KEYM');
        sigsToProcess.add('AMMO');
        sigsToProcess.add('ARMO');
        sigsToProcess.add('BOOK');
        sigsToProcess.add('WEAP');
        sigsToProcess.add('ALCH');
        sigsToProcess.add('NOTE');
        
        gameProgramList := TStringList.create;
        gameProgramList.add('atomiccommand.swf');
        gameProgramList.add('grognak.swf');
        gameProgramList.add('pipfall.swf');
        gameProgramList.add('zetainvaders.swf');
        gameProgramList.add('redmenace.swf');
        gameProgramList.add('automatron\automatron.swf');

        weaponTypesMelee := TStringList.create;
        weaponTypesMelee.add('WeaponTypeMelee1H');
        weaponTypesMelee.add('WeaponTypeUnarmed');
        weaponTypesMelee.add('WeaponTypeMelee2H');

        //AddMessage('All lists loaded');

        loadDefaultINNRs();
        //AddMessage('Loaded Default INNRs');

        settingAddComponentString := true;
    end;

    procedure cleanupTagifier();
    begin
        //clean
        if(assigned(fileToClean)) then begin
            CleanMasters(fileToClean);
        end;
        
        if(extraValidTagsList <> nil) then begin
            extraValidTagsList.free();
        end;
    
        scriptBlackList.free();
        scriptBlackListPrefix.free();
        edidBlackListPrefix.free();
        sigsToProcess.free();
        gameProgramList.free();

        cleanUpLists();
    end;

    procedure loadDefaultINNRs();
    var
        innrRoot: IInterface;
        mainFile: IInterface;
    begin
        mainFile := findFile('Fallout4.esm');
        if(not assigned(mainFile)) then begin
            AddMessage('Fallout4.esm missing');
            exit;
        end;
        // load stuff
        innrRoot := GroupBySignature(mainFile, 'INNR');


        innrCommonMelee := MainRecordByEditorID(innrRoot, 'dn_CommonMelee');
        innrCommonArmor := MainRecordByEditorID(innrRoot, 'dn_CommonArmor');
        innrPowerArmor  := MainRecordByEditorID(innrRoot, 'dn_PowerArmor');
        innrClothes     := MainRecordByEditorID(innrRoot, 'dn_Clothes');
        innrCommonGun   := MainRecordByEditorID(innrRoot, 'dn_CommonGun');
        innrVaultSuit   := MainRecordByEditorID(innrRoot, 'dn_VaultSuit');
    end;

    function isEdidIgnored(edid: string): boolean;
    var
        i: integer;
        curEdid, edidLC: string;
    begin
        Result := true;
        if(IsItemIgnored(edid)) then exit;

        Result := hasPrefixInList(edid, edidBlackListPrefix);
        {
        edidLC := LowerCase(edid);
        for i:=0 to edidBlackListPrefix.count-1 do begin
            curEdid := LowerCase(edidBlackListPrefix[i]);

            if(curEdid = edidLC) then exit;
            if(strStartsWithCI(edidLC, curEdid)) then exit;
        end;


        Result := false;
        }
    end;

    function getWinningOverrideExcept(e: IInterface; targetFile: IInterface): IInterface;
    var
        i: integer;
    begin
    end;

    function tagifyElement(e: IInterface; toFile: IInterface): IInterface;
    var
      curSig: String;
      newName: String;
      curName: String;
      scrapComponentsString: String;
      newElem, curOverride, existingOverride, winOverride, masterElem: IInterface;
      curEdid: String;
      i, numOverrides: integer;
    begin
        Result := nil;
        curSig := Signature(e);

        if (sigsToProcess.indexOf(curSig) < 0) then begin
            // AddMessage('Skipping, signature');
            exit;
        end;

        // AddMessage('Checking '+FullPath(e));
        // check blacklist right here
        curEdid := GetElementEditValues(e, 'EDID');
        if(isEdidIgnored(curEdid)) then begin
            // AddMessage('Skipping '+curName+', EDID');
            exit;
        end;

        if (
            hasAnyScript(e, scriptBlackList) or
            hasAnyScriptInPrefixList(e, scriptBlackListPrefix)
        ) then begin
            // AddMessage('Skipping '+curName+', Script');
            exit;
        end;
        // AddMessage('Script ok');

        curName := DisplayName(e);
        if(curName = '') then begin
            exit;
        end;


        masterElem   := MasterOrSelf(e);
        numOverrides := OverrideCount(masterElem);
        for i:=0 to numOverrides-1 do begin
            curOverride := OverrideByIndex(masterElem, i);

            if (FilesEqual(GetFile(curOverride), toFile)) then begin
                existingOverride := curOverride;
                break;
            end else begin
                winOverride := curOverride;
            end;
        end;

        if(not assigned(winOverride)) then begin
            winOverride := e;
        end;


        curName := DisplayName(winOverride);

        if(curName = '') then begin
            exit;
        end;


        if(assigned(existingOverride)) then begin
            if(isElementUnsaved(existingOverride)) then begin
                AddMessage('Skipping, Processed ' + curName+' already');
                exit;
            end;
        end;

        if not checkItemTag(curName) then begin
            
            fileToClean := toFile;

            scrapComponentsString := getComponents(winOverride);

            newName := GetOverrideForName(winOverride, curEdid, curName, scrapComponentsString);

            if (newName = '') then begin

                // try this
                newName := getNameFromExistingOverride(winOverride, curEdid, curName, scrapComponentsString);
                if(newName = '') then begin
                    if curSig = 'MISC' then begin
                        //AddMessage('== Processing MISC');
                        newName := processMisc(winOverride, scrapComponentsString);
                        //AddMessage('== Processing MISC done');
                    end else begin

                        if curSig = 'KEYM' then begin
                            newName := prefixNameWithTag(tagKey, curName);
                        end else if curSig = 'AMMO' then begin
                            if(not hasNonPlayableFlag(winOverride)) then begin
                                newName := prefixNameWithTag(tagAmmo, curName);
                            end;
                        end else if curSig = 'ARMO' then begin
                            // AddMessage('== Processing ARMO');
                            processArmo(winOverride);
                            // AddMessage('== Processing ARMO done');
                        end else if curSig = 'BOOK' then begin
                            // AddMessage('== Processing NOTE');
                            newName := processNote(winOverride);
                            // AddMessage('== Processing NOTE done');
                        end else if curSig = 'WEAP' then begin
                            // AddMessage('== Processing WEAP');
                            newName := processWeap(winOverride);
                            // AddMessage('== Processing WEAP done');
                        end else if curSig = 'ALCH' then begin
                            //AddMessage('== Processing ACLH');
                            newName := processAlch(winOverride);
                            //AddMessage('== Processing ACLH done');
                        end else if curSig = 'NOTE' then begin
                            newName := processHolotape(winOverride);
                        end;
                    end;
                end;
            end;


            if newName <> '' then begin
                AddMessage('Renaming: '+curName+ ' -> '+newName);
                // addRequiredMastersSilent(winOverride, ToFile);
                newElem := getOrCreateElementOverride(winOverride, ToFile);
                SetElementEditValues(newElem, 'FULL', newName);
                Result := newElem;
            end;
        end;
    end;

end.