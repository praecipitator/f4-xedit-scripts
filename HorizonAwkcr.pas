{
    Attempts to copy some of Horizon's recipes to the various AWKCR benches.
    As opposed to Horizonifier, this can and should be run on Horizon itself.
}
unit HorizonAwkcr;

    uses praUtil;
    uses praFunctions;
    
    var
        setupDone: boolean;
        akcrFile: IwbFile;
       // horiFile: IwbFile;
        architectFile: IwbFile;
        ToFile: IwbFile;
        
        explosiveKeyword: IInterface;
        ammoKeyword: IInterface;
        weaponKeyword: IInterface;
        armorKeyword: IInterface;
        
        weaponLabKeyword : IInterface;
        techLabKeyword : IInterface;
        
        ammoKeywords: TStringList;
        
    function getNewCobjEdid(oldEdid: String): string;
    begin
        result := 'AWKCR_HZ_AUTO_'+oldEdid;
    end;
        
    function doSetup(): boolean;
    begin
        
        akcrFile := findFile('ArmorKeywords.esm');
        // horiFile := findFile('Z_Horizon.esp');
        architectFile := findFile('Z_Architect.esm');
        
        if(not assigned(akcrFile)) then begin
            AddMessage('ArmorKeywords.esm not found!');
            Result := false;
            exit;
        end;
        
        if(not assigned(architectFile)) then begin
            AddMessage('Z_Architect.esm not found!');
            Result := false;
            exit;
        end;
        
        // relevant KWs from Horizon: 
        // WorkbenchWeaponsLab [KYWD:07122192] = weapon lab
        // WorkbenchRoboticsLab [KYWD:07004CEE] = tech lab
        weaponLabKeyword  := MainRecordByEditorID(GroupBySignature(architectFile, 'KYWD'), 'WorkbenchWeaponsLab_Arc');
        techLabKeyword := MainRecordByEditorID(GroupBySignature(architectFile, 'KYWD'), 'WorkbenchTechLab_Arc');
        
        { weaponLabKeyword := MainRecordByEditorID(GroupBySignature(horiFile, 'KYWD'), 'WorkbenchWeaponsLab'); }
        { techLabKeyword := MainRecordByEditorID(GroupBySignature(horiFile, 'KYWD'), 'WorkbenchRoboticsLab'); }
        
        // relevant KWs from AWKCR
        ammoKeyword := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_ck_AmmunitionCraftingKey');
        armorKeyword := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_ck_ArmorsmithCraftingKey'); 
        explosiveKeyword := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_ck_ExplosivesCraftingKey');
        weaponKeyword := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_ck_WeaponsmithCraftingKey');
        
        ammoKeywords := TStringList.create;
        ammoKeywords.add('RecipeWeaponLab_Ammo_Arc'); //ballistic
        ammoKeywords.add('RecipeWeaponLab_AmmoSpecial_Arc');// unused??
        ammoKeywords.add('RecipeWeaponLab_AmmoExt_Arc');//special
        ammoKeywords.add('RecipeWeaponLab_AmmoEnergy_Arc');//energy
        ammoKeywords.add('RecipeWeaponLab_AmmoContracted_Arc');//contract
        ammoKeywords.add('RecipeWeaponLab_AmmoHeavy_Arc');//heavy
        ammoKeywords.add('RecipeTech_Scrap');//disassembly
        
        setupDone := true;
        Result := true;
        
    end;

    
    function getCopy(origCobj: IInterface): IInterface;
    var
        oldEdid: String;
        newEdid: String;
        
        
    begin
        oldEdid := GetElementEditValues(origCobj, 'EDID');
        newEdid := getNewCobjEdid(oldEdid);
        
        Result := MainRecordByEditorID(GroupBySignature(ToFile, 'COBJ'), newEdid);
        if(not assigned(Result)) then begin
            addRequiredMastersSilent(origCobj, ToFile);
            Result := wbCopyElementToFile(origCobj, ToFile, True, True);
            setElementEditValues(Result, 'EDID', newEdid);
        end;
    end;
    
    procedure copyToBench(cobj: IInterface; newBnam: IInterface);
    var
        newCobj: IInterface;
    begin
        newCobj := getCopy(cobj);
        addRequiredMastersSilent(newBnam, ToFile);
        setBnam(newCobj, newBnam);
        
    end;
    
    procedure processWeaponlab(e: IInterface);
    var 
        origName: String;
    begin
        // AddMessage('Weaponlab');
        origName := DisplayName(getCraftResult(e));
        // mines and grenades go to explosive
        // ammo and disassembly go to ammo workbench
        // weapons go to weapons
        // equpment goes to armor
        if(hasKeywordBySignature(e, 'RecipeGrenade', 'FNAM')) then begin
            AddMessage('Copying '+origName+' to explosives workbench');
            copyToBench(e, explosiveKeyword);
        end else if(hasKeywordBySignature(e, 'RecipeMines', 'FNAM')) then begin
            AddMessage('Copying '+origName+' to explosives workbench');
            copyToBench(e, explosiveKeyword);
        end else if(hasAnyKeyword(e, ammoKeywords, 'FNAM')) then begin
            AddMessage('Copying '+origName+' to ammo workbench');
            copyToBench(e, ammoKeyword);
        end else if(hasKeywordBySignature(e, 'RecipeRobotics_Weapons_Arc', 'FNAM')) then begin
            AddMessage('Copying '+origName+' to weapon workbench');
            copyToBench(e, weaponKeyword);
        end else if(hasKeywordBySignature(e, 'RecipeRobotics04_Arc', 'FNAM')) then begin // equipment
            AddMessage('Copying '+origName+' to armor workbench');
            copyToBench(e, armorKeyword);
        end else;
    end;
    
    procedure processTechlab(e: IInterface);
    begin
        //AddMessage('techlab');
    end;
    
    procedure processCobj(e: IInterface);
    var
        bnam: IInterface;
    begin
        bnam := getBnam(e);
        if(isSameForm(bnam, weaponLabKeyword)) then begin
            processWeaponlab(e);
            exit;
        end;
        
        if(isSameForm(bnam, techLabKeyword)) then begin
            processTechlab(e);
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
        
        if(not setupDone) then begin
            if(not doSetup()) then begin
                Result := 1;
                exit;
            end;
        end;
        
        if(curSig = 'COBJ') then begin
            processCobj(e);
        end;
    end;
end.