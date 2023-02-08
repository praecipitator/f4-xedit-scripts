{
    Restores precombines, previs, and optionally navmeshes to the state of the selected masters.
    Run this script on cells or worldspaces which you want restored. Select options in the UI.
}
unit userscript;
    uses praUtil;
    const
        configFile = ProgramPath + 'Edit Scripts\Restore Precombines.cfg';

    var
        masterCheckList: TCheckListBox;
        doPhysicsRefs: boolean;
        restoreCell: boolean;
        restorePrecombines: boolean;
        restoreLandscape: boolean;
        restoreNavmeshes: boolean;
        restorePrecombFiles: boolean;
        selectedNames: TStringList;
        lastTargetFileName: string;
        targetFile: IInterface;
        containerListMasters: TwbFastStringList;
        resourceListMasters: TwbFastStringList;
        resourceListOther: TwbFastStringList;

    procedure loadConfig();
    var
        i, j, breakPos: integer;
        curLine, curKey, curVal: string;
        lines : TStringList;
    begin
        // default
        restoreCell := true;
        restorePrecombines := true;
        doPhysicsRefs := true;
        restoreLandscape := true;
        restorePrecombFiles := true;
        restoreNavmeshes := false;

        lastTargetFileName := '';

        if(not FileExists(configFile)) then begin
            exit;
        end;
        lines := TStringList.create;
        lines.LoadFromFile(configFile);

        //
        for i:=0 to lines.count-1 do begin
            curLine := lines[i];
            breakPos := -1;

            for j:=1 to length(curLine) do begin
                if(curLine[j] = '=') then begin
                    breakPos := j;
                    break;
                end;
            end;

            if breakPos <> -1 then begin
                curKey := trim(copy(curLine, 0, breakPos-1));
                curVal := trim(copy(curLine, breakPos+1, length(curLine)));

                if(curKey = 'restorePrecombines') then begin
                    restorePrecombines := StrToBool(curVal);
                end else if(curKey = 'restoreCell') then begin
                    restoreCell := StrToBool(curVal);
                end else if(curKey = 'doPhysicsRefs') then begin
                    doPhysicsRefs := StrToBool(curVal);
                end else if(curKey = 'restoreLandscape') then begin
                    restoreLandscape := StrToBool(curVal);
                end else if(curKey = 'restoreNavmeshes') then begin
                    restoreNavmeshes := StrToBool(curVal);
                end else if(curKey = 'restorePrecombFiles') then begin
                    restorePrecombFiles := StrToBool(curVal);
                end else if(curKey = 'lastTargetFileName') then begin
                    lastTargetFileName := curVal;
                end else if(curKey = 'selectedFiles') then begin
                    selectedNames.Delimiter := ';';
                    selectedNames.StrictDelimiter := TRUE;
                    selectedNames.DelimitedText := curVal;
                end;
            end;
        end;

        lines.free();
    end;

    procedure saveConfig();
    var
        lines : TStringList;
    begin
        lines := TStringList.create;


        lines.add('restorePrecombines='+BoolToStr(restorePrecombines));
        lines.add('restoreCell='+BoolToStr(restoreCell));
        lines.add('doPhysicsRefs='+BoolToStr(doPhysicsRefs));
        lines.add('restoreLandscape='+BoolToStr(restoreLandscape));
        lines.add('restoreNavmeshes='+BoolToStr(restoreNavmeshes));
        lines.add('restorePrecombFiles='+BoolToStr(restorePrecombFiles));
        lines.add('lastTargetFileName='+lastTargetFileName);
        if(selectedNames.count > 0) then begin
            selectedNames.Delimiter := ';';
            lines.add('selectedFiles='+ selectedNames.DelimitedText);
        end;


        lines.saveToFile(configFile);
        lines.free();
    end;


    procedure menuSelectAllHandler(sender: TObject);
    begin
        masterCheckList.CheckAll(cbChecked, False, False);
    end;

    procedure menuUnSelectAllHandler(sender: TObject);
    begin
        masterCheckList.CheckAll(cbUnChecked, False, False);
    end;

    procedure menuInvertSelectionHandler(sender: TObject);
    var
        i: integer;
    begin
        for i:=0 to masterCheckList.Items.count-1 do begin
            masterCheckList.Checked[i] := not masterCheckList.Checked[i];
        end;
    end;

    procedure menuSelectMastersHandler(sender: TObject);
    var
        i: integer;
        curName: string;
    begin
        for i:=0 to masterCheckList.Items.count-1 do begin
            curName := masterCheckList.Items[i];
            masterCheckList.Checked[i] := strEndsWithCI(curName, '.esm');
        end;
    end;

    procedure setupMenu(list: TListBox);
    var
        selectAllItem, selectNoneItem, invertSelectionItem, selectMastersItem: TMenuItem;
        menu: TPopupMenu;
    begin
        menu  := TPopupMenu.Create(list);
        //menu.Name := 'wtf';
        list.PopupMenu  := menu;
        //menu.onPopup := menuOpenHandler;

        selectAllItem := TMenuItem.create(list.PopupMenu);
        selectAllItem.Name := 'selectAllItem';
        selectAllItem.caption := 'Select All';
        selectAllItem.onclick := menuSelectAllHandler;

        selectNoneItem := TMenuItem.create(list.PopupMenu);
        selectNoneItem.Name := 'selectNoneItem';
        selectNoneItem.caption := 'Select None';
        selectNoneItem.onclick := menuUnSelectAllHandler;

        invertSelectionItem := TMenuItem.create(list.PopupMenu);
        invertSelectionItem.Name := 'invertSelectionItem';
        invertSelectionItem.caption := 'Invert Selection';
        invertSelectionItem.onclick := menuInvertSelectionHandler;

        selectMastersItem := TMenuItem.create(list.PopupMenu);
        selectMastersItem.Name := 'selectMastersItem';
        selectMastersItem.caption := 'Select ESMs only';
        selectMastersItem.onclick := menuSelectMastersHandler;

        menu.Items.add(selectAllItem);
        menu.Items.add(selectNoneItem);
        menu.Items.add(invertSelectionItem);
        menu.Items.add(selectMastersItem);
    end;

    function showConfigDialog(): boolean;
    var
        frm: TForm;
        btnOkay, btnCancel: TButton;
        windowHeightBase, windowWidthBase, topOffset: integer;
        resultCode, i, newIndex: integer;
        cbDoPhysics, cbRestoreLandscape, cbRestoreNavmesh, cbRestorePrecombs, cbRestoreCell, cbRestorePrecombFiles: TCheckBox;
        curFile: IwbFile;
        curFileName: string;
        targetFileSelector: TComboBox;
    begin
        loadConfig();
        Result := false;
        windowHeightBase := 400;
        windowWidthBase := 360;
        topOffset := 4;
        frm := CreateDialog('Restore Precombines', windowWidthBase, windowHeightBase+60);

        CreateLabel(frm, 4, topOffset, 'Restore precombines of these masters:');

        topOffset := topOffset + 18;
        btnOkay := CreateButton(frm, 10, windowHeightBase, 'OK');
        btnOkay.ModalResult := mrYes;
        btnOkay.width := 75;

        btnCancel := CreateButton(frm, 90, windowHeightBase, 'Cancel');
        btnCancel.ModalResult := mrCancel;
        btnCancel.width := 75;

        masterCheckList := TCheckListBox.create(frm);
        masterCheckList.Parent := frm;
        masterCheckList.Width := windowWidthBase-16;
        masterCheckList.Height := 200;
        masterCheckList.Left := 4;
        masterCheckList.Top := topOffset;

        setupMenu(masterCheckList);

        topOffset := masterCheckList.Height + 30;

        cbRestorePrecombs := CreateCheckbox(frm, 10, topOffset, 'Restore Precombines');
        if(restorePrecombines) then begin
            cbRestorePrecombs.state := cbChecked;
        end;

        topOffset := topOffset + 20;
        cbRestoreCell := CreateCheckbox(frm, 10, topOffset, 'Restore Cell Itself');
        if(restoreCell) then begin
            cbRestoreCell.state := cbChecked;
        end;

        topOffset := topOffset + 20;
        cbDoPhysics := CreateCheckbox(frm, 10, topOffset, 'Restore Physics References');
        if(doPhysicsRefs) then begin
            cbDoPhysics.state := cbChecked;
        end;
        topOffset := topOffset + 20;
        cbRestoreLandscape := CreateCheckbox(frm, 10, topOffset, 'Restore Landscapes');
        if(restoreLandscape) then begin
            cbRestoreLandscape.state := cbChecked;
        end;
        topOffset := topOffset + 20;
        cbRestoreNavmesh := CreateCheckbox(frm, 10, topOffset, 'Restore Navmeshes');
        if(restoreNavmeshes) then begin
            cbRestoreNavmesh.state := cbChecked;
        end;
        topOffset := topOffset + 20;
        cbRestorePrecombFiles := CreateCheckbox(frm, 10, topOffset, 'Restore Precombined Files');
        if(restorePrecombFiles) then begin
            cbRestorePrecombFiles.state := cbChecked;
        end;

        topOffset := topOffset + 30;

        CreateLabel(frm, 4, topOffset+2, 'Target File:');
        targetFileSelector := CreateComboBox(frm, 80, topOffset, 250, nil);
        targetFileSelector.Style := csDropDownList;
        targetFileSelector.Items.Add('-- CREATE NEW FILE --');
        targetFileSelector.ItemIndex := 0;


        // fill
        for i := 0 to FileCount-1 do begin
            curFile := FileByIndex(i);
            curFileName := GetFileName(curFile);
            if(not strEndsWithCI(curFileName, '.exe')) then begin
                // target file list
                newIndex := targetFileSelector.Items.AddObject(curFileName, curFile);
                if(lastTargetFileName = curFileName) then begin
                    targetFileSelector.ItemIndex := newIndex;
                end;

                // master checkbox list
                newIndex := masterCheckList.Items.AddObject(curFileName, curFile);

                if(selectedNames.count = 0) then begin
                    if(strEndsWithCI(curFileName, '.esm')) then begin
                        masterCheckList.Checked[newIndex] := true;
                    end;
                end else begin
                    if(selectedNames.indexOf(curFileName) >= 0) then begin
                        masterCheckList.Checked[newIndex] := true;
                    end;
                end;
            end;
        end;


        resultCode := frm.ShowModal;

        if(resultCode <> mrYes) then begin
            Result := false;
            frm.free();
            exit;
        end;

        if(targetFileSelector.ItemIndex = 0) then begin
            targetFile := AddNewFile();
        end else begin
            targetFile := ObjectToElement(targetFileSelector.Items.Objects[targetFileSelector.ItemIndex]);
        end;

        if(not assigned(targetFile)) then begin
            Result := 1;
            exit;
        end;

        lastTargetFileName := GetFileName(targetFile);

        restorePrecombines := (cbRestorePrecombs.state = cbChecked);
        doPhysicsRefs := (cbDoPhysics.state = cbChecked);
        restoreLandscape := (cbRestoreLandscape.state = cbChecked);
        restoreNavmeshes := (cbRestoreNavmesh.state = cbChecked);
        restorePrecombFiles := (cbRestorePrecombFiles.state = cbChecked);
        restoreCell := (cbRestoreCell.state = cbChecked);

        selectedNames.clear();
        for i:=0 to masterCheckList.Items.count-1 do begin
            if(masterCheckList.Checked[i]) then begin
                curFileName := masterCheckList.Items[i];
                selectedNames.addObject(curFileName, ObjectToElement(masterCheckList.Items.Objects[i]));
            end;
        end;


        Result := true;

        saveConfig();
        frm.free();
    end;

    function ExtractFileBasename(filename: string): string;
    var
        curExt: string;
    begin
        curExt := ExtractFileExt(filename);

        Result := copy(filename, 0, length(filename)-length(curExt));
    end;

    procedure fillContainerLists();
    var
        resContainerList, curResources: TwbFastStringList;
        i, j, p: integer;
        curContainer, basename, cntName, cntType, curName, targetFileBase: string;
        curContainerIsMaster: boolean;
    begin
        resContainerList := TwbFastStringList.create;
        containerListMasters := TwbFastStringList.create;
        resourceListMasters := TwbFastStringList.create;
        resourceListOther := TwbFastStringList.create;

        resourceListMasters.Sorted := True;
        resourceListMasters.Duplicates := dupIgnore;
        resourceListMasters.CaseSensitive := false;

        resourceListOther.Sorted := True;
        resourceListOther.Duplicates := dupIgnore;
        resourceListOther.CaseSensitive := false;

        ResourceContainerList(resContainerList);

        targetFileBase := ExtractFileBasename(GetFileName(targetFile));

        for i:=0 to resContainerList.count-1 do begin
            curContainer := resContainerList[i];
            baseName := ExtractFileBasename(getStringAfter(curContainer, 'Data\'));
            if(baseName = '') then continue;

            p := Pos(' - ', baseName);
            if(p <= 0) then continue;

            cntName := copy(baseName, 1, p-1);
            cntType := LowerCase(copy(baseName, p+3, length(basename)-p-2));

            if (cntType <> 'main') and (cntType <> 'meshes') and (cntType <> 'meshesextra') then begin
                continue;
            end;

            if(LowerCase(cntName) = LowerCase(targetFileBase)) then begin
                continue;
            end;

            curContainerIsMaster := false;
            for j:=0 to selectedNames.count-1 do begin
                curName := ExtractFileBasename(selectedNames[j]);
                if(LowerCase(curName) = LowerCase(cntName)) then begin
                    // found relevant master container
                    containerListMasters.add(curContainer);
                    curContainerIsMaster := true;
                    break;
                end;
            end;

            curResources := TwbFastStringList.create;

            ResourceList(curContainer, curResources);

            if(curContainerIsMaster) then begin
                resourceListMasters.addStrings(curResources);
            end else begin
                resourceListOther.addStrings(curResources);
            end;

            curResources.free();
        end;
        resContainerList.free();
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        selectedNames := TStringList.create;
        Result := 0;

        if(not showConfigDialog()) then begin
            selectedNames.free();
            Result := 1;
            exit;
        end;

        containerListMasters := nil;
        resourceListMasters := nil;
        resourceListOther := nil;


        if(restorePrecombFiles) then begin
            fillContainerLists();

        end;
    end;

    function getLatestRelevantMaster(e: IInterface): IInterface;
    var
        i, num: integer;
        curOverride, theMaster: IInterface;
        curFilename: string;
    begin
        theMaster := MasterOrSelf(e);
        Result := nil;
        num := OverrideCount(theMaster);

        for i:=num-1 downto 0 do begin
            curOverride := OverrideByIndex(theMaster, i);
            curFilename := GetFileName(GetFile(curOverride));

            if(selectedNames.indexOf(curFileName) >= 0) then begin
                Result := curOverride;
                exit;
            end;
        end;


        curFilename := GetFileName(theMaster);

        if(selectedNames.indexOf(curFileName) >= 0) then begin
            Result := theMaster;
            exit;
        end;
    end;

    function getElementNames(e: IInterface): TStringList;
    var
        i: integer;
        curElem: IInterface;
        curName: string;
    begin
        Result := TStringList.create;
        for i := 0 to ElementCount(e)-1 do begin
            curElem := ElementByIndex(e, i);
            // use name
            Result.add(Name(curElem));
        end;
    end;

    procedure overwriteElement(srcElem, targetElem: IInterface);
    var
        i: integer;
        curElem, newEntry: IInterface;
        namesSrc, namesTarget, namesSrcOnly, namesTargetOnly, namesBoth: TStringList;
        curName, curValSrc, curValTarget: string;
    begin

        namesSrc := getElementNames(srcElem);
        namesTarget := getElementNames(targetElem);

        namesSrcOnly := TStringList.create;
        namesTargetOnly := TStringList.create;
        namesBoth := TStringList.create;

        for i := 0 to namesSrc.count-1 do begin
            curName := namesSrc[i];
            //AddMessage('curName='+curName);
            if(namesTarget.IndexOf(curName) < 0) then begin
                // not present in target
                namesSrcOnly.add(curName);
            end else begin
                namesBoth.add(curName);
            end;
        end;

        for i := 0 to namesTarget.count-1 do begin
            curName := namesTarget[i];
            if(namesSrc.indexOf(curName) < 0) then begin
                // not contained in src
                namesTargetOnly.add(curName);
            end;
        end;

        namesSrc.free();
        namesTarget.free();

        // for the namesBoth, copy over
        for i:=0 to namesBoth.count-1 do begin
            curName := namesBoth[i];
            //AddMessage('curName = '+curName);
            if(curName = 'Record Header') then begin

                SetFormVCS1(targetElem, GetFormVCS1(srcElem));
                SetFormVCS2(targetElem, GetFormVCS2(srcElem));

                SetElementEditValues(targetElem, 'Record Header\Record Flags', GetElementEditValues(srcElem, 'Record Header\Record Flags'));
            end else begin
                //curValSrc := GetElementEditValues(srcElem, curName);
                //curValTarget := GetElementEditValues(targetElem, curName);

                wbCopyElementToRecord(ElementByPath(srcElem, curName), targetElem, false, true);
            end;
        end;

        // for namesSrcOnly, create in target
        for i:=0 to namesSrcOnly.count-1 do begin
            curName := namesSrcOnly[i];
            AddMessage('Trying to add '+curName);

            wbCopyElementToRecord(ElementByPath(srcElem, curName), targetElem, false, true);
        end;

        // for namesTargetOnly, remove them
        for i:=0 to namesTargetOnly.count-1 do begin
            curName := namesTargetOnly[i];
            //AddMessage('Trying to rem '+curName);
            RemoveElement(targetElem, curName);
        end;



        namesSrcOnly.free();
        namesTargetOnly.free();
        namesBoth.free();
    end;


    procedure restoreElement(e, targetFile: IInterface; doCheck: boolean);
    var
        srcElem, targetElem, otherOverride: IInterface;
    begin
        srcElem := getLatestRelevantMaster(e);
        if(not assigned(srcElem)) then exit;

        if(doCheck) then begin
            otherOverride := getWinningOverrideBefore(e, targetFile);
            if(not assigned(otherOverride)) then exit; // no need

            if(Equals(srcElem, otherOverride)) then exit; // also no need
        end;

        targetElem := getOrCreateElementOverride(e, targetFile);
        // targetElem := getExistingElementOverride(e, targetFile);
        if(not assigned(targetElem)) then begin
            // try this for cells
            if(Signature(e) = 'CELL') then begin
                if(ElementsEquivalent(srcElem, otherOverride)) then begin
                    AddMessage('Skipping cell: ' + FullPath(srcElem));
                    // no need
                    exit;
                end;
            end;
            targetElem := createElementOverride(e, targetFile);
        end;
        //createElementOverride(curRef, GetFile(e));
        //curRef := getExistingElementOverride(curRef, GetFile(e));

        AddMessage('Restoring: ' + FullPath(srcElem));
        overwriteElement(srcElem, targetElem);
    end;

    procedure processRefsWithinCell(cell: IInterface);
    var
        tmp, r: IInterface;
        j: integer;
    begin
        tmp := FindChildGroup(ChildGroup(cell), 9, cell);
        for j := 0 to ElementCount(tmp)-1 do begin
            r := ElementByIndex(tmp, j);
            // if(restoreLandscape or restoreNavmeshes) then begin
            if(restoreLandscape) then begin
                if(Signature(r) = 'LAND') then begin
                    restoreElement(r, targetFile, true);
                end;
            end;

            if(restoreNavmeshes) then begin
                if(Signature(r) = 'NAVM') then begin
                    restoreElement(r, targetFile, true);
                end;
            end;
        end;
    end;

    procedure tryToRestoreFile(theFilename: string);
    var
        i: integer;
        curContainer, foundMasterContainer: string;
        curAssets : TwbFastStringList;
    begin
        // AddMessage('Will try to restore '+theFilename);
        // now this is hard
        if(resourceListMasters.IndexOf(theFilename) < 0) then begin
            // AddMessage('> not in any master');
            exit;
        end;
        if(resourceListOther.IndexOf(theFilename) < 0) then begin
            // AddMessage('> not overridden in any other');
            exit;
        end;

        // now figure out which master do we have this in
        foundMasterContainer := '';
        for i:=containerListMasters.count-1 downto 0 do begin
            curContainer := containerListMasters[i];
            curAssets := TwbFastStringList.create();
            curAssets.Sorted := True;
            curAssets.Duplicates := dupIgnore;
            curAssets.CaseSensitive := false;


            ResourceList(curContainer, curAssets);

            if(curAssets.indexOf(theFilename) >= 0) then begin
                // found
                curAssets.free();
                foundMasterContainer := curContainer;

                break;
            end;

            curAssets.free();
        end;

        if(foundMasterContainer = '') then begin
            AddMessage('!!! something bad broke');
            exit;
        end;

        AddMessage('Restoring '+theFilename+' from '+foundMasterContainer);
        // copy theFilename from foundMasterContainer to data
        ResourceCopy(foundMasterContainer, theFilename, DataPath);
    end;

    procedure restoreFilesForCell(cell: IInterface);
    var
        filesToRestore: TStringList;
        previsRoot, cellMaster, cellMasterFile, xcri, meshes: IInterface;
        previsFileName, masterName, formIdHex, physicsName, meshName: string;
        i: integer;
    begin

        cellMaster := MasterOrSelf(cell);
        cellMasterFile := GetFile(cellMaster);
        masterName := GetFileName(cellMasterFile);
        formIdHex := IntToHex(getElementLocalFormId(cellMaster) and $00FFFFFF, 8);
        //filesToRestore := TStringList.create();

        // we need:
        // - the uvd file, if this is a previs root
        previsRoot := PathLinksTo(cell, 'RVIS');
        if(assigned(previsRoot)) then begin
            if(IsSameForm(previsRoot, cell)) then begin
                previsFileName := 'Vis\'+masterName+'\'+formIdHex+'.uvd';
                tryToRestoreFile(previsFileName);
            end;
        end;
        // - the physics file
        if(LowerCase(masterName) = 'fallout4.esm') then begin
            physicsName := 'Meshes\PreCombined\'+formIdHex+'_Physics.NIF';
        end else begin
            physicsName := 'Meshes\PreCombined\'+masterName+'\'+formIdHex+'_Physics.NIF';
        end;
        tryToRestoreFile(physicsName);

        // - all the PreCombined files
        xcri := ElementBySignature(cell, 'XCRI');
        if(assigned(xcri)) then begin
            meshes :=  ElementByName(xcri, 'Meshes');

            for i:=0 to ElementCount(meshes)-1 do begin
                meshName := GetEditValue(ElementByIndex(meshes, i));
                tryToRestoreFile('Meshes\'+meshName);
            end;
        end;

        //filesToRestore.free();
    end;


    procedure restorePrecombsForCell(e: IInterface);
    var
        srcElem, relevantOverride: IInterface;
        i: integer;
        xrpi, xcri, curEntry, curRef: IInterface;
        cellChildGroup: IInterface;
    begin
        srcElem := getLatestRelevantMaster(e);
        if(not assigned(srcElem)) then exit;

        // relevantOverride := getExistingElementOverrideOrClosest(e, targetFile);
        // if(not assigned(relevantOverride)) then exit;


        // AddMessage('Restoring cell: ' + FullPath(e));
        // overwrite e using srcElem
        if(restoreCell) then begin
            restoreElement(e, targetFile, true);
        end;

        if(restorePrecombines) then begin
            xcri := ElementByPath(e, 'XCRI\References');
            for i:=0 to ElementCount(xcri)-1 do begin
                curEntry := ElementByIndex(xcri, i);
                curRef := pathLinksTo(curEntry, 'Reference');

                restoreElement(curRef, targetFile, true);
            end;
        end;

        // now do all the precombined refs
        if(doPhysicsRefs) then begin
            xrpi := ElementByPath(e, 'XPRI');
            if(assigned(xrpi)) then begin
                for i:=0 to ElementCount(xrpi)-1 do begin
                    curRef := LinksTo(ElementByIndex(xrpi, i));

                    restoreElement(curRef, targetFile, true);
                end;
            end;
        end;

        if(restoreLandscape or restoreNavmeshes) then begin
            processRefsWithinCell(srcElem);
        end;

        if(restorePrecombFiles) then begin
            restoreFilesForCell(srcElem);
        end;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        if(Signature(e) = 'CELL') then begin
            if(not GetIsPersistent(e)) then begin
                restorePrecombsForCell(e);
            end;
        end;
    end;


    procedure cleanUp();
    begin
        selectedNames.free();
        if(resourceListMasters <> nil) then begin
            resourceListMasters.free();
        end;

        if(resourceListOther <> nil) then begin
            resourceListOther.free();
        end;

        if(containerListMasters <> nil) then begin
            containerListMasters.free();
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        cleanUp();
        Result := 0;
    end;

end.