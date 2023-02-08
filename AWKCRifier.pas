{
    Moves construction recipes from the chemistry workbench to the various AWKCR workbenches
}
unit AWKCRifier;

    uses praUtil;
    uses praFunctions;

    var
        ToFile: IInterface;
        setupDone: boolean;
        setupSuccessful: boolean;
        
        // keywords
        explosiveKeyword: IInterface;
        ammoKeyword: IInterface;
        weaponKeyword: IInterface;
        armorKeyword: IInterface;
        workbenchChemlabKw: IInterface;
        workbenchElectronicsKw: IInterface;
        workbenchGeneralKw: IInterface;
        
        utilityCategoryKw: IInterface; // RecipeUtility
        utility2CategoryKw: IInterface; // RecipeMisc
        
        unconvAmmoKw: IInterface; // AEC_cm_AL_Unconventional_Ammo
        unconvWeapKw: IInterface; // AEC_cm_WG_Unconventional_Weapons_Recipe
        otherRecipeKw: IInterface; // AEC_cm_Other_Recipe
        
        
        holotapeKw: IInterface;
        deviceKw: IInterface;
        toolsKw: IInterface;
        
        akcrFile: IwbFile;
        mainFile: IwbFile;
        //elwbFile: IwbFile;
        
        kwListDevice: TStringList;
        kwExtraBenchesBlacklist: TStringList;
        
        modelListDevice: TStringList;
        modelListTool: TStringList;
        modelExtraBenchesBlacklist: TStringList;
        
        soundListFood: TStringList;
        soundListDevice: TStringList;
        soundListTools: TStringList;
        
        replaceableFnamList: TStringList;
        
        extraDeviceIDs: TStringList;
        
        chemicalScrapList: TStringList;
  

    function doSetup(e: IInterface): integer;
    var
        frm: TForm;
        i: integer;
        clb: TCheckListBox;
    begin
    
        Result := 0;
        // AddMasterIfMissing
        if (not Assigned(ToFile)) then begin
            frm := frmFileSelect;
            try
                frm.Caption := 'Select a plugin';
                clb := TCheckListBox(frm.FindComponent('CheckListBox1'));
                clb.Items.Add('<new file>');
                for i := Pred(FileCount) downto 0 do
                    if GetFileName(e) <> GetFileName(FileByIndex(i)) then
                        clb.Items.InsertObject(1, GetFileName(FileByIndex(i)), FileByIndex(i))
                    else
                        Break;
                    if frm.ShowModal <> mrOk then begin
                        Result := 1;
                        Exit;
                    end;
                for i := 0 to Pred(clb.Items.Count) do
                    if clb.Checked[i] then begin
                        if i = 0 then 
                            ToFile := AddNewFile 
                        else
                            ToFile := ObjectToElement(clb.Items.Objects[i]);
                        Break;
                    end;
            finally
                frm.Free;
            end;
            if not Assigned(ToFile) then begin
                Result := 1;
                Exit;
     
            end;
        end;
        
        // all the awkcr stuff
        ammoKeyword := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_ck_AmmunitionCraftingKey');
        armorKeyword := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_ck_ArmorsmithCraftingKey'); 
        explosiveKeyword := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_ck_ExplosivesCraftingKey');
        weaponKeyword := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_ck_WeaponsmithCraftingKey');
        
        unconvAmmoKw := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_cm_AL_Unconventional_Ammo');
        unconvWeapKw := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_cm_WG_Unconventional_Weapons_Recipe');
        otherRecipeKw:= MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_cm_Other_Recipe');
    
        workbenchElectronicsKw := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_ck_ElectronicsCraftingKey');
        holotapeKw := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_cm_Holotapes');
        deviceKw := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_cm_Devices');
        
        workbenchGeneralKw := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_ck_UtilityCraftingKey');
        toolsKw := MainRecordByEditorID(GroupBySignature(akcrFile, 'KYWD'), 'AEC_cm_Tools');
        
        
        // stuff from vanilla
        workbenchChemlabKw:= MainRecordByEditorID(GroupBySignature(mainFile, 'KYWD'), 'WorkbenchChemlab');
        utilityCategoryKw :=  MainRecordByEditorID(GroupBySignature(mainFile, 'KYWD'), 'RecipeUtility'); 
        utility2CategoryKw :=  MainRecordByEditorID(GroupBySignature(mainFile, 'KYWD'), 'RecipeMisc'); 

        
        setupDone := true;
        
    end;
    
    function isModelIn(e: IInterface; list: TStringList): boolean;
    var
        model: string;
    begin
        Result := (list.indexOf(getModelName(e)) >= 0);
    end;
    
    function containsChemicalComponents(e: IInterface): boolean;
    var
        numComponents: integer;

        j: integer;
        
        componentMain: IInterface;
        componentRoot: IInterface;
        component: IInterface;
        componentValue: IInterface;
        quantity: integer;
        
        curId: String;
    begin
        Result := false;
        componentRoot := ElementByName(e, 'CVPA - Components');
        if componentRoot <> nil then begin
            numComponents := ElementCount(componentRoot);
                    
            if numComponents > 0 then begin
                for j := 0 to numComponents-1 do begin
                    componentMain := ElementByIndex(componentRoot, j);
                   
                    component := ElementByName(componentMain, 'Component');
                    //AddMessage('component foo ');
                    if component <> nil then begin
                        component := LinksTo(component);

                        
                        componentValue := ElementByName(componentMain, 'Count');
                        
                        quantity := StrToInt(GetEditValue(componentValue));
                        if quantity > 0 then begin
                            
                            curId := GetElementEditValues(component, 'EDID');
                            //AddMessage('id is '+curId);
                            if(chemicalScrapList.indexOf(curId) >= 0) then begin
                                Result := true;
                                exit;
                            end;
                        end
                    end;
                    
                end;
            end;
        end;
    end;

    
    function processElectronics(e: IInterface; bnamKw: IInterface; createdObj: IInterface): boolean;
    var
        sig: String;
        flags : IInterface;
        enit: IInterface;
        newType: integer;
        flagsVal: integer;
        newElem: IInterface;
        tmpElem: IInterface;
        deviceType : integer;
    begin
        Result := false;
        newType := 0; // 1 := holotape, 2 := device, 3 := tool, 4 := generic craftable
        
        // if this is in any awkcr bench already, don't continue
        if (isSameForm(bnamKw, workbenchElectronicsKw)) then begin
            Result := true;
            exit;
        end;
        
        sig := Signature(createdObj);
        
        if(extraDeviceIDs.indexOf(GetElementEditValues(e, 'EDID')) >= 0) then begin
            newType := 2;
        end else begin
            
            if(sig = 'NOTE') then begin
                newType := 1;
            end else if(sig = 'ALCH') then begin
                
            
                //deviceType := isAlchDevice(createdObj);
                deviceType := getAlchemyType(createdObj);
              
                
                if(deviceType = 40) then begin
                    newType := 2;//device
                end else if(deviceType = 41) then begin
                    newType := 3;//tool
                end;
            end else if(sig = 'WEAP') then begin
                // if in utility, move to tools
                if(hasKeywordBySignature(e, 'RecipeUtility', 'FNAM')) then begin 
                    newType := 3;
                end;
            end else if(sig = 'MISC') then begin
                // move any non-scrap stuff to electronics
                
                if( hasAnyKeyword(createdObj, kwExtraBenchesBlacklist, 'KWDA') ) then begin
                    Result := false;
                    exit;
                end;
                
                // modelExtraBenchesBlacklist

                tmpElem := ElementBySignature(createdObj, 'CVPA');
                if((not assigned(tmpElem)) or (elementCount(tmpElem) < 0)) then begin
                    
                    // 
                    if(not isModelIn(createdObj, modelExtraBenchesBlacklist)) then begin
                        // if(modelListDevice.indexOf(getModelName(createdObj)) >= 0) then begin
                        if(getModelName(createdObj) = 'Props\Holotape_Prop.nif') then begin
                            newType := 1;
                        end else begin
                            newType := 2;
                        end;
                    end;
                    
                end else begin
                    // AddMessage('has components');
                    // move to generic
                    // todo add more checks
                    //not isModelIn(createdObj, modelExtraBenchesBlacklist) and
                    if(not containsChemicalComponents(createdObj)) then begin 
                        newType := 4;
                    end;
                end;
            end;
        end;

        
        if(newType > 0) then begin
            
            
            addRequiredMastersSilent(e, ToFile);
            addRequiredMastersSilent(workbenchElectronicsKw, ToFile);
            newElem := getOverrideForElem(e, ToFile);
            
                
            if(newType = 1) then begin
                AddMessage('Moving '+DisplayName(createdObj)+' to Electronics Workbench');
                setBnam(newElem, workbenchElectronicsKw);
                // holotape
                replaceAnyFnam(newElem, replaceableFnamList, holotapeKw, true);
                //replaceFnam(newElem, utilityCategoryKw, holotapeKw, true);
                Result := true;
            end else if(newType=2) then begin
                AddMessage('Moving '+DisplayName(createdObj)+' to Electronics Workbench');
                setBnam(newElem, workbenchElectronicsKw);
                replaceAnyFnam(newElem, replaceableFnamList, deviceKw, true);
                Result := true;
            end else if(newType=3) then begin
                AddMessage('Moving '+DisplayName(createdObj)+' to Utility Workbench');
                setBnam(newElem, workbenchGeneralKw);
                replaceAnyFnam(newElem, replaceableFnamList, toolsKw, true);
                Result := true;
            end else if(newType=4) then begin
                AddMessage('Moving '+DisplayName(createdObj)+' to Utility Workbench');
                setBnam(newElem, workbenchGeneralKw);
                // no setting the category
                Result := true;
            end;
        end;
        
    end;
    
    function processAwkcr(e: IInterface; bnamKw: IInterface; createdObj: IInterface): boolean;
    var
        newElem: IInterface;
        newType: integer;
        sig: String;
    begin 
        newType := 0; // 1 = mine/grenade, 2 = ammo, 3 = weapon, 4 = armor
      
        Result := false;
        sig := Signature(createdObj);

        if (hasKeywordBySignature(e, 'RecipeMines', 'FNAM') or hasKeywordBySignature(e, 'RecipeGrenade', 'FNAM')) then begin
            // no matter anything else, if it's in the chemistry bench and either a mine or a grenade, move it to explosives
            // well, one exception: ammo
            if(sig <> 'AMMO') then begin
                newType := 1;
            end;
        end else if (hasKeywordBySignature(e, 'RecipeThrowingTrap', 'FNAM')) then begin
            // move all traps from the chemistry workbench to weapons
            newType := 3;
        end;
    
        if (newType = 0) then begin
            
            
            if(sig = 'WEAP') then begin 
                if (hasKeywordBySignature(createdObj, 'WeaponTypeExplosive','KWDA') or hasKeywordBySignature(createdObj, 'WeaponTypeGrenade','KWDA')) then begin
                    //AddMessage('explo');
                    newType := 1;
                end else if(not hasKeywordBySignature(e, 'RecipeUtility', 'FNAM')) then begin
                    // do NOT move weapons out of utility. Only out of other stuff
                    newType := 3;
                end;
                
                
            end else if (sig = 'AMMO') then begin 
                newType := 2;
            end else if (sig = 'ARMO') then begin 
                newType := 4;
            end;
        end;
     
        
        if(newType > 0) then begin 
            
            addRequiredMastersSilent(e, ToFile);
            addRequiredMastersSilent(explosiveKeyword, ToFile);
            newElem := getOverrideForElem(e, ToFile);
        
            if(newType = 1) then begin
                AddMessage('Moving '+DisplayName(createdObj)+' to Explosives Workbench');
                setBnam(newElem, explosiveKeyword);
                replaceFnam(newElem, utilityCategoryKw, otherRecipeKw, true);
            end else if(newType = 2) then begin
                AddMessage('Moving '+DisplayName(createdObj)+' to Ammunition Workbench');
                setBnam(newElem, ammoKeyword);
                replaceFnam(newElem, utilityCategoryKw, unconvAmmoKw, true);
            end else if(newType = 3) then begin
                AddMessage('Moving '+DisplayName(createdObj)+' to Weapon Workbench');
                setBnam(newElem, weaponKeyword);
                replaceFnam(newElem, utilityCategoryKw, unconvWeapKw, true);
            end else if(newType = 4) then begin
                AddMessage('Moving '+DisplayName(createdObj)+' to Armor Workbench');
                setBnam(newElem, armorKeyword);
                replaceFnam(newElem, utilityCategoryKw, otherRecipeKw, true);
            end;
            Result := true;
        end;
   
    end;
    
    
    function processCobj(e: IInterface): IInterface;
    var
        bnam: IInterface;
        bnamKw: IInterface;
        createdObj: IInterface;
        doneThisElem: boolean;
    begin 
        createdObj := getCraftResult(e);
        doneThisElem := false;
        
        
        bnamKw := getBnam(e);

        

        if(assigned(bnamKw)) then begin
            // only ever process the chemlab. it's safer this way.
            if(isSameForm(bnamKw, workbenchChemlabKw)) then begin 

                if(doneThisElem = false) then begin
                    doneThisElem := processElectronics(e, bnamKw, createdObj);
                end;
                
                if(doneThisElem = false) then begin
                    doneThisElem := processAwkcr(e, bnamKw, createdObj);
                end;
            end;
        end;
        
      
    end;
    
    


    function Initialize: integer;
    begin
        setupSuccessful := true;
        akcrFile := findFile('ArmorKeywords.esm');
        // elwbFile := findFile('ExtraWorkbenches.esp');
        mainFile := findFile('Fallout4.esm');

        // hasExtraWbs := (Assigned(elwbFile));
        
        if(not Assigned(mainFile)) then begin
            setupSuccessful := false;
            AddMessage('ERROR: Fallout4.esm not found!');
        end;
        
        if(not (Assigned(akcrFile))) then begin
            AddMessage('ERROR: ArmorKeywords.esm not found. There is noting this script can do.');
            setupSuccessful := false;
        end else begin
            AddMessage('Found ArmorKeywords.esm');
        end;
        
        

        setupDone := false;
        
        kwListDevice := TStringList.create;
        kwExtraBenchesBlacklist := TStringList.create;
        modelExtraBenchesBlacklist := TStringList.create;
        modelListDevice := TStringList.create;
        modelListTool := TStringList.create;
        
        soundListFood := TStringList.create;
        soundListDevice := TStringList.create;
        soundListTools := TStringList.create;
        
        replaceableFnamList := TStringList.create;
        
        extraDeviceIDs := TStringList.create;
        
        chemicalScrapList := TStringList.create;
        
        kwListDevice.add('ChemTypeStealthBoy');
        kwListDevice.add('StealthBoyKeyword');
        kwListDevice.add('DLC01ObjectTypeRepairKit');
        
        
        kwExtraBenchesBlacklist.add('ObjectTypeChem');
        kwExtraBenchesBlacklist.add('ObjectTypeFood');
        kwExtraBenchesBlacklist.add('ObjectTypeWater');
        kwExtraBenchesBlacklist.add('ObjectTypeStimpak');
        kwExtraBenchesBlacklist.add('ObjectTypeSyringerAmmo');
        kwExtraBenchesBlacklist.add('FoodEffect');
        kwExtraBenchesBlacklist.add('HC_DiseaseRisk_FoodStandard');
        kwExtraBenchesBlacklist.add('ObjectTypeDrink');
        kwExtraBenchesBlacklist.add('ObjectTypeCaffeinated');
        kwExtraBenchesBlacklist.add('CA_ObjType_ChemBad');
        kwExtraBenchesBlacklist.add('FruitOrVegetable');
        kwExtraBenchesBlacklist.add('HC_CausesImmunodeficiency');
        kwExtraBenchesBlacklist.add('HC_SustenanceType_IncreasesHunger');
        kwExtraBenchesBlacklist.add('HC_DiseaseRisk_FoodHigh');
        kwExtraBenchesBlacklist.add('HC_DiseaseRisk_FoodStandard');
        kwExtraBenchesBlacklist.add('HC_SustenanceType_QuenchesThirst');
        kwExtraBenchesBlacklist.add('DLC05FireworkShellKeyword');
        kwExtraBenchesBlacklist.add('TempComponentTeflon');//mostly to prevent moving of that BoS item
        
        modelListDevice.add('Props\StealthBoy01.nif');
        modelListDevice.add('DLC01\Props\DLC01_RobotRepairKit01.nif');
        modelListDevice.add('Props\BS101RadioTransmittor.nif');
        modelListDevice.add('Props\BoSDistressPulser\BoSDistressPulserGlowing.nif');
        modelListDevice.add('Props\BoSCerebroFusionAdaptor\BoSCerebroFusionAdaptor.nif');
        modelListDevice.add('Actors\LibertyPrime\CharacterAssets\Agitator.nif');
        modelListDevice.add('Props\BoSDistressPulser\BoSDistressPulser.nif');
        modelListDevice.add('Props\BoSReflexCapacitor\BoSReflexCapacitor.nif');
        modelListDevice.add('Props\BoSHapticDrive\BoSHapticDrive.nif');
        modelListDevice.add('Props\BoSFluxSensor\BoSFluxSensor.nif');
        modelListDevice.add('Props\GenericKeycard01.nif');
        modelListDevice.add('Props\MilitaryCircuitBoard\MilitaryCircuitBoard.nif');
        modelListDevice.add('Props\PreWar_Toaster.nif');
        modelListDevice.add('Props\PostWar_Toaster.nif');
        modelListDevice.add('SetDressing\Building\DeskFanOffice01.nif');
        modelListDevice.add('SetDressing\Building\DeskFanOfficeOff01.nif');
        modelListDevice.add('DLC01\Props\DLC01_ComponentPart.nif');
        modelListDevice.add('DLC01\Props\DLC01_ComponentWhole.nif');
        modelListDevice.add('DLC01\Props\DLC01_Amplifier01.nif');
        modelListDevice.add('Props\SynthChip\SynthChip.nif');
        modelListDevice.add('DLC03\Props\DLC03_FogCondenserPowerModule.nif');
        modelListDevice.add('Props\MS11PowerRelayCoil.nif');
        modelListDevice.add('Props\Component\Component_Circuitry.nif');
        modelListDevice.add('Props\BioMedicalScanner\BioMedicalScanner.nif');
        modelListDevice.add('Props\Camera.nif');
        modelListDevice.add('Props\Fuse01.nif');
        modelListDevice.add('Props\FusionPulseCharge\FusionPulseCharge.nif');
        modelListDevice.add('Props\BOS_Magnet.nif');
        modelListDevice.add('Props\HotPlate.nif');
        modelListDevice.add('Props\MS11RadarTransmittor.nif');
        modelListDevice.add('SetDressing\LightFixtures\LightbulbOff.nif');
        modelListDevice.add('Props\Chipboard.nif');
        modelListDevice.add('SetDressing\Quest\GenPrototype01.nif');
        modelListDevice.add('Props\VacuumTube01.nif');
        modelListDevice.add('Props\StealthBoy01.nif');
        modelListDevice.add('Weapons\Grenade\TransmitGrenadeProjectile.nif');
        modelListDevice.add('Props\PipboyMiscItem\PipboyMisc01.nif');
        modelListDevice.add('DLC06\Props\PipboyMiscItem\DLC06PipboyMisc01.nif');
        modelListDevice.add('SetDressing\Factions\Railroad\TinkerTomsDevice01.nif');
        
        modelListTool.add('AutoBuildPlots\Weapons\Hammer\Hammer.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolHammer01A.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolHammer01B.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolHammer02.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolHammer03.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolSaw01.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolSaw02.nif');
        modelListTool.add('Props\SmithingTools\SmithingToolTongs01.nif');
        modelListTool.add('Props\SurgicalTools\SurgicalCutter.nif');
        modelListTool.add('Props\SurgicalTools\SurgicalScalpel.nif');
        modelListTool.add('Props\SurgicalTools\SurgicalScissors.nif');
        modelListTool.add('Props\BlowTorch.nif');
        modelListTool.add('Props\BlowTorch_Rare.nif');
        
        
        modelExtraBenchesBlacklist.add('Props\Stimpack01.nif');
        modelExtraBenchesBlacklist.add('Props\BOSCompound.nif');
        modelExtraBenchesBlacklist.add('Props\Jet.nif');
        modelExtraBenchesBlacklist.add('Props\Mentats.nif');
        modelExtraBenchesBlacklist.add('Props\BuffOut.nif');
        modelExtraBenchesBlacklist.add('Props\MedX.nif');
        modelExtraBenchesBlacklist.add('Props\Pyscho.nif');
        modelExtraBenchesBlacklist.add('Props\SyringeAmmo.nif');
        // negative? , 
        
        
        
        soundListFood.add('NPCHumanEatSoup');
        soundListFood.add('NPCHumanDrinkGeneric');
        soundListFood.add('NPCHumanEatChewy');
        soundListFood.add('NPCHumanEatGeneric');
        soundListFood.add('NPCHumanChemsPsycho');
        soundListFood.add('NPCHumanEatMentats');
        soundListFood.add('NPCHumanDrinkGeneric');
        soundListFood.add('NPCHumanChemsUseJet');
        soundListFood.add('NPCHumanChemsAddictol');
        soundListFood.add('NPCHumanEatEgg');
        soundListFood.add('NPCHumanEatSoupSlurp');
        
        soundListDevice.add('OBJStealthBoyActivate');
        soundListTools.add('NPCHumanWhistleDog');
        
        
        // KWs which can be replaced in FNAM, for electronics at least
        replaceableFnamList.add('RecipeUtility');
        replaceableFnamList.add('RecipeMisc');
        replaceableFnamList.add('RecipeHealing');
        replaceableFnamList.add('RecipeGrenade');
        replaceableFnamList.add('RecipeMines');
        
        // if any of these are craftable, move them to devices
        extraDeviceIDs.add('AlarmClock');
        extraDeviceIDs.add('AlarmClock_Rare');
        extraDeviceIDs.add('c_Circuitry_scrap');
        extraDeviceIDs.add('BiometricScanner');
        extraDeviceIDs.add('Camera');
        extraDeviceIDs.add('Camera_Clean');
        extraDeviceIDs.add('Camera_Rare');
        extraDeviceIDs.add('CircuitBoardAssaultron');
        extraDeviceIDs.add('CircuitBoardMilitary');
        extraDeviceIDs.add('ClothingIron');
        
        chemicalScrapList.add('c_Acid');
        chemicalScrapList.add('c_Antiseptic');
        chemicalScrapList.add('c_Refrigerant');// mod added but meh
        
    end;

    function Process(e: IInterface): integer;
    var
        i: integer;
        curFile: IwbFile;
    begin
        if (not setupSuccessful) then begin
            Result := 1;
            exit;
        end;
        // only process COBJs
        if Signature(e) <> 'COBJ' then
            Exit;
        
        if (not setupDone) then begin
            if (doSetup(e) <> 0) then begin
                exit;
            end;
        end;
        
        curFile := GetFile(e);
        if(GetFileName(curFile) = GetFileName(akcrFile)) then begin
            AddMessage('Entry '+GetElementEditValues(e, 'EDID')+' is from AWKCR, skipping.');
            exit;
        end;
        
        {if(hasExtraWbs and (GetFileName(curFile) = GetFileName(elwbFile))) then begin
            AddMessage('Entry '+GetElementEditValues(e, 'EDID')+' is from ExtraWorkbenches, skipping.');
            exit;
        end;}
        
        
        
        processCobj(e);
        
        // BNAM - Workbench Keyword

        //AddRequiredElementMasters(e, ToFile, False);
        
        // 
    end;


end.