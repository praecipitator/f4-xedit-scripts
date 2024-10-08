{
  New script template, only shows processed records
  Assigning any nonzero value to Result will terminate script
}
unit robotModLib;

    var
        praSS2Bot_Bot_FortifyStrength,
        praSS2Bot_Bot_FortifyPerception,
        praSS2Bot_Bot_FortifyEndurance,
        praSS2Bot_Bot_FortifyCharisma,
        praSS2Bot_Bot_FortifyIntelligence,
        praSS2Bot_Bot_FortifyAgility,
        praSS2Bot_Bot_FortifyLuck: IInterface;

        effectTemplate: IInterface;
        targetFile: IInterface;

    procedure setEffect(elem, av: IInterface; val: integer; isFirst: boolean);
    var
        effects, curEff: IInterface;
    begin
        effects := ElementByPath(elem, 'Effects');

        if(isFirst) then begin
            curEff := ElementByIndex(effects, 0);
        end else begin
            curEff := ElementAssign(effects, HighInteger, nil, False);
        end;

        SetPathLinksTo(curEff, 'EFID', av);
        SetElementEditValues(curEff, 'EFIT\Magnitude', IntToStr(val));
    end;

    function getEnchantment(strength, perception, endurance, charisma, intelligence, agility, luck: integer): IInterface;
    var
        edidBase, nameBase: string;
        doneFirst: boolean;
    begin
        Result := nil;

        edidBase := 'praSS2Bot_EnchBot';
        nameBase := '';
        if(strength <> 0) then begin
            edidBase := edidBase+'_S'+IntToStr(strength);
            nameBase := nameBase+'Strength '+IntToStr(strength)+' ';
        end;
        if(perception <> 0) then begin
            edidBase := edidBase+'_P'+IntToStr(perception);
            nameBase := nameBase+'Perception '+IntToStr(perception)+' ';
        end;
        if(endurance <> 0) then begin
            edidBase := edidBase+'_E'+IntToStr(endurance);
            nameBase := nameBase+'Endurance '+IntToStr(endurance)+' ';
        end;
        if(charisma <> 0) then begin
            edidBase := edidBase+'_C'+IntToStr(charisma);
            nameBase := nameBase+'Charisma '+IntToStr(charisma)+' ';
        end;
        if(intelligence <> 0) then begin
            edidBase := edidBase+'_I'+IntToStr(intelligence);
            nameBase := nameBase+'Intelligence '+IntToStr(intelligence)+' ';
        end;
        if(agility <> 0) then begin
            edidBase := edidBase+'_A'+IntToStr(agility);
            nameBase := nameBase+'Agility '+IntToStr(agility)+' ';
        end;
        if(luck <> 0) then begin
            edidBase := edidBase+'_L'+IntToStr(luck);
            nameBase := nameBase+'Luck '+IntToStr(luck)+' ';
        end;

        Result := FindObjectByEdid(edidBase);
        if (assigned(Result)) then begin
            exit;
        end;

        Result := wbCopyElementToFile(effectTemplate, targetFile, True, True);
        SetElementEditValues(Result, 'EDID', edidBase);
        SetElementEditValues(Result, 'FULL', trim(nameBase));
        //      AddMessage('Creating');
        //      FullPath(Result);

        // clearing effects doesn't work, there must be one
        doneFirst := true;

        if(strength <> 0) then begin
            setEffect(Result, praSS2Bot_Bot_FortifyStrength, strength, doneFirst);
            doneFirst := false;
        end;

        if(perception <> 0) then begin
            setEffect(Result, praSS2Bot_Bot_FortifyPerception, perception, doneFirst);
            doneFirst := false;
        end;

        if(endurance <> 0) then begin
            setEffect(Result, praSS2Bot_Bot_FortifyEndurance, endurance, doneFirst);
            doneFirst := false;
        end;

        if(charisma <> 0) then begin
            setEffect(Result, praSS2Bot_Bot_FortifyCharisma, charisma, doneFirst);
            doneFirst := false;
        end;

        if(intelligence <> 0) then begin
            setEffect(Result, praSS2Bot_Bot_FortifyIntelligence, intelligence, doneFirst);
            doneFirst := false;
        end;

        if(agility <> 0) then begin
            setEffect(Result, praSS2Bot_Bot_FortifyAgility, agility, doneFirst);
            doneFirst := false;
        end;

        if(luck <> 0) then begin
            setEffect(Result, praSS2Bot_Bot_FortifyLuck, luck, doneFirst);
            doneFirst := false;
        end;

    end;

    procedure doInit(toFile: IInterface);
    begin
        praSS2Bot_Bot_FortifyStrength := FindObjectByEdid('praSS2Bot_Bot_FortifyStrength');
        praSS2Bot_Bot_FortifyPerception := FindObjectByEdid('praSS2Bot_Bot_FortifyPerception');
        praSS2Bot_Bot_FortifyEndurance := FindObjectByEdid('praSS2Bot_Bot_FortifyEndurance');
        praSS2Bot_Bot_FortifyCharisma := FindObjectByEdid('praSS2Bot_Bot_FortifyCharisma');
        praSS2Bot_Bot_FortifyIntelligence := FindObjectByEdid('praSS2Bot_Bot_FortifyIntelligence');
        praSS2Bot_Bot_FortifyAgility := FindObjectByEdid('praSS2Bot_Bot_FortifyAgility');
        praSS2Bot_Bot_FortifyLuck := FindObjectByEdid('praSS2Bot_Bot_FortifyLuck');

        effectTemplate := FindObjectByEdid('praSS2Bot_EnchBot_E1');
        if(not assigned(effectTemplate)) then begin
            AddMessage('=== ERROR: FAILED TO FIND EFFECT TEMPLATE ===');
        end;

        targetFile := toFile;
    end;

    function intToSignedStr(i: integer): string;
    begin
        Result := '';
        if(i > 0) then begin
            Result := '+' + IntToStr(i);
            exit;
        end

        Result := IntToStr(i);
    end;

    procedure setStats(orig: IInterface; strength, perception, endurance, charisma, intelligence, agility, luck: integer);
    begin
        setStatsDesc(orig, strenth, perception, endurance, charisma, intelligence, agility, luck, '');
    end;

    function resetOverride(e: IInterface): IInterface;
    var
        masterRec, masterFile: IInterface;
    begin
        masterRec := MasterOrSelf(e);
        e := getExistingElementOverride(masterRec, targetFile);
        Result := e;
        if(isSameFile(GetFile(masterRec), getFile(e))) then begin
            exit;
        end;

        // masterFile := GetFile(masterRec);
        // AddMessage('should be removing');
        Remove(e);

        // addRequiredMastersSilent(masterRec, targetFile);
        // Result := wbCopyElementToFile(masterRec, targetFile, False, True);
    end;

    procedure setStatsDesc(orig: IInterface; strength, perception, endurance, charisma, intelligence, agility, luck: integer; description: string);
    var
        ench, overr, props, newProp: IInterface;
        oldDesc, newDescPart, newDesc: string;
    begin
        orig := MasterOrSelf(orig);
        if (strength = 0) and (perception = 0) and (endurance = 0) and (charisma = 0) and (intelligence = 0) and (agility = 0) and (luck = 0) then begin
            // if this is an override, remove it
            //AddMessage('all zero');
            if(not isSameFile(getFile(orig), targetFile)) then begin
                // remove orig, create new override
                //AddMessage('should reset');
                resetOverride(orig);
            end;
            // otherwise, dunno yet
            exit;
        end;


        ench := getEnchantment(strength, perception, endurance, charisma, intelligence, agility, luck);
        if(not assigned(ench)) then begin
            AddMessage('ERROR: no enchantment for '+IntToStr(strength)+' '+IntToStr(perception)+' '+IntToStr(endurance)+' '+IntToStr(charisma)+' '+IntToStr(intelligence)+' '+IntToStr(agility)+' '+IntToStr(luck));
            exit;
        end;

        if(isSameFile(getFile(orig), targetFile)) then begin
            overr := orig;
            // here, we also need to erase the properties
            props := ElementByPath(overr, 'DATA\Properties');
            while(ElementCount(props) > 0) do begin
                RemoveElement(props, 0);
            end;

            //RemoveElement(overr, 'DATA\Properties');
            //ensurePath(overr, 'DATA\Properties');
        end else begin
            orig := getWinningOverrideBefore(orig, targetFile);
            overr := createElementOverride(orig, targetFile);
        end;

        props := ElementByPath(overr, 'DATA\Properties');
        newProp := ElementAssign(props, HighInteger, nil, False);

        SetElementEditValues(newProp, 'Value Type', 'FormID,Int');
        SetElementEditValues(newProp, 'Function Type', 'ADD');
        SetElementEditValues(newProp, 'Property', 'Enchantments');
        // SetElementEditValues(newProp, 'Value 1 - FormID', 'Enchantments');
        SetElementEditValues(newProp, 'Value 2 - Int', '1');
        setPathLinksTo(newProp, 'Value 1 - FormID', ench);

        if (description = '') then begin
            oldDesc := GetElementEditValues(overr, 'DESC');
        end else begin
            oldDesc := description;
        end;

        newDescPart := '';

        newDesc := oldDesc;
        if(oldDesc <> '') then begin
            newDesc := newDesc + ' ';
        end;

        if(strength <> 0) then begin
            newDesc := newDesc + intToSignedStr(strength)+' Strength ';
        end;

        if(perception <> 0) then begin
            newDesc := newDesc + intToSignedStr(perception)+' Perception ';
        end;

        if(endurance <> 0) then begin
            newDesc := newDesc + intToSignedStr(endurance)+' Endurance ';
        end;

        if(charisma <> 0) then begin
            newDesc := newDesc + intToSignedStr(charisma)+' Charisma ';
        end;

        if(intelligence <> 0) then begin
            newDesc := newDesc + intToSignedStr(intelligence)+' Intelligence ';
        end;

        if(agility <> 0) then begin
            newDesc := newDesc + intToSignedStr(agility)+' Agility ';
        end;

        if(luck <> 0) then begin
            newDesc := newDesc + intToSignedStr(luck)+' Luck ';
        end;

        SetElementEditValues(overr, 'DESC', trim(newDesc)+'.');
    end;

end.