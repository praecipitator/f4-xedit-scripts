{
    to be run on cells in the target file only
}
unit userscript;
    uses praUtil;

    const
        configFile = ScriptsPath + 'FixXPRI.cfg';

    var
        settingWorkshopString: string;
        settingTargetLayer: string;
        settingLinkToWorkshop: boolean;
        settingScraplinkToNew: boolean;
        settingMoveToLayer: boolean;

        targetWorkshop: IInterface;
        targetLayer: IInterface;

        WorkshopStackedItemParentKEYWORD: IInterface;
        WorkshopLinkContainer: IInterface;


    procedure loadConfig();
    var
        i, j, breakPos: integer;
        curLine, curKey, curVal: string;
        lines : TStringList;
    begin
        // default
        settingLinkToWorkshop := false;
        settingScraplinkToNew := false;
        settingMoveToLayer := false;


        settingTargetLayer := '';
        settingWorkshopString := '';

        if(not FileExists(configFile)) then begin
            exit;
        end;
        lines := TStringList.create;
        lines.LoadFromFile(configFile);

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

                if(curKey = 'WorkshopString') then begin
                    settingWorkshopString := curVal;
                end else if(curKey = 'LinkToWorkshop') then begin
                    settingLinkToWorkshop := StrToBool(curVal);
                end else if(curKey = 'MoveToLayer') then begin
                    settingMoveToLayer := StrToBool(curVal);
                end else if(curKey = 'ScraplinkToNew') then begin
                    settingScraplinkToNew := StrToBool(curVal);
                end else if(curKey = 'TargetLayer') then begin
                    settingTargetLayer := curVal;
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
        lines.add('WorkshopString='+settingWorkshopString);
        lines.add('TargetLayer='+settingTargetLayer);

        lines.add('LinkToWorkshop='+BoolToStr(settingLinkToWorkshop));
        lines.add('ScraplinkToNew='+BoolToStr(settingScraplinkToNew));
        lines.add('MoveToLayer='+BoolToStr(settingMoveToLayer));

        lines.saveToFile(configFile);
        lines.free();
    end;


    function showConfigDialog(): boolean;
    var
        frm: TForm;
        cbScraplinkToNew, cbLinkToWorkshop, cbMoveToLayer: TCheckBox;

        editLayer, editWorkshopStr: TEdit;
        resultCode: cardinal;
        btnOkay, btnCancel: TButton;
        yOffset: integer;
    begin
        //loadConfig();
        Result := false;
        frm := CreateDialog('Fix XPRI Settings', 350, 300);

        cbScraplinkToNew := CreateCheckbox(frm, 10, 10, 'Scraplink to new ref');
        if(settingScraplinkToNew) then begin
            cbScraplinkToNew.checked := true;
        end;

        yOffset := 20;

        cbLinkToWorkshop := CreateCheckbox(frm, 10, yOffset+ 10, 'Link containers to Workshop');
        if(settingLinkToWorkshop) then begin
            cbLinkToWorkshop.checked := true;
        end;
        yOffset := yOffset + 20;

        CreateLabel(frm, 10, yOffset + 20, 'Workshop ref to use:');
        editWorkshopStr := CreateInput(frm, 10, yOffset + 40, settingWorkshopString);
        editWorkshopStr.width := 260;
        yOffset := yOffset + 80;



        cbMoveToLayer := CreateCheckbox(frm, 10, yOffset, 'Move deleted to layer');
        cbMoveToLayer.checked := settingMoveToLayer;

        CreateLabel(frm, 10, yOffset + 23, 'Layer:');
        editLayer := CreateInput(frm, 10, yOffset + 40, settingTargetLayer);
        editLayer.width := 260;
        yOffset := yOffset + 80;


        btnOkay := CreateButton(frm, 90, yOffset, '  OK  ');
        btnOkay.ModalResult := mrYes;
        btnOkay.Default := true;

        btnCancel := CreateButton(frm, 180, yOffset, 'Cancel');
        btnCancel.ModalResult := mrCancel;

        resultCode := frm.showModal();

        if(resultCode = mrYes) then begin
            settingScraplinkToNew := cbScraplinkToNew.checked;
            settingLinkToWorkshop := cbLinkToWorkshop.checked;
            settingMoveToLayer    := cbMoveToLayer.checked;

            settingWorkshopString := trim(editWorkshopStr.text);
            settingTargetLayer    := trim(editLayer.text);
            saveConfig();


            Result := true;
            if(settingLinkToWorkshop) then begin
                if(settingWorkshopString <> '') then begin
                    targetWorkshop := findFormByString(settingWorkshopString);
                    if(not assigned(targetWorkshop)) then begin
                        AddMessage('Failed to find workshop "'+settingWorkshopString+'"');
                        Result := false;
                    end else begin
                        if(Signature(targetWorkshop) <> 'REFR') then begin
                            AddMessage('Form "'+settingWorkshopString+'" is not a REFR');
                            Result := false;
                        end;
                    end;
                end;
            end;

            if(settingMoveToLayer) then begin
                if(settingTargetLayer <> '') then begin
                    targetLayer := FindObjectByEdidAndSignature(settingTargetLayer, 'LAYR');
                    if(not assigned(targetLayer)) then begin
                        AddMessage('Failed to find layer "'+targetLayer+'"');
                        Result := false;
                    end;
                end;
            end;


        end;


        frm.free();
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 1;
        WorkshopLinkContainer := AbsStrToForm('Fallout4.esm:0002682F');
        if(not assigned(WorkshopLinkContainer)) then begin
            AddMessage('Failed to find WorkshopLinkContainer');
            exit;
        end;

        WorkshopStackedItemParentKEYWORD := AbsStrToForm('Fallout4.esm:001C5EDD');
        if(not assigned(WorkshopStackedItemParentKEYWORD)) then begin
            AddMessage('Failed to find WorkshopStackedItemParentKEYWORD');
            exit;
        end;

        Result := 0;
        loadConfig();
        if( not showConfigDialog()) then begin
            Result := 1;
            exit;
        end;
    end;

    procedure replaceFormIn(container, search, repl, targetFile: IInterface);
    var
        i: integer;
        child, lt: IInterface;
    begin

        lt := LinksTo(container);
        if(assigned(lt)) then begin
            //lt := getOrCreateElementOverride(lt, targetFile);
            if(IsSameForm(lt, search)) then begin
                //AddMessage('Found one');
                //AddMessage('  '+FullPath(container));
                setLinksTo(container, repl);
            end;
        end;

        for i := 0 to ElementCount(container)-1 do begin

            child := ElementByIndex(container, i);
            // AddMessage(prefix+DisplayName(child)+'='+GetEditValue(child));
            replaceFormIn(child, search, repl, targetFile);
        end;
    end;

    procedure replaceFormUsages(formSearch, formReplace, targetFile: IInterface);
    var
        i, j, numRefs: cardinal;
        curRef, curRefOverride: IInterface;
        searchEdid: string;
    begin

        AddMessage('trying to replace '+Name(formSearch)+' with '+Name(formReplace));
        for i := ReferencedByCount(formSearch)-1 downto 0 do begin
            curRef := ReferencedByIndex(formSearch, i);
            if(Signature(curRef) = 'TES4') then continue;
            AddMessage(' -> trying to do it in '+Name(curRef));
            curRefOverride := getOrCreateElementOverride(curRef, targetFile);

            replaceFormIn(curRefOverride, formSearch, formReplace, targetFile);
        end;

    end;

    procedure markAsDeleted(e: IInterface);
    var
        baseForm: IInterface;
        baseFormSig: string;
        prevOverride, xesp: IInterface;
        prevZpos: integer;
        isRefMaster, inDummyPrecomb, canDummyPrecomb, newPersistence: boolean;
    begin



        prevOverride := nil;
        isRefMaster := isMaster(e);
        if(not isRefMaster) then begin
            prevOverride := getWinningOverrideBefore(e, GetFile(e));
        end;

        RemoveElement(e, 'Enable Parent');
        RemoveElement(e, 'XTEL');
        // ... remove anything else here
        // linked refs maybe
        RemoveElement(e, 'Linked References');
        // just in case
        RemoveElement(e, 'XLRL');
        RemoveElement(e, 'XLRT');

        //if(shouldChangePersist) then begin
            // maybe unpersist
        if(isRefMaster) then begin
            // yes
            newPersistence := false;
            // SetIsPersistent(e, False);
        end else if (assigned(prevOverride)) then begin
            // apply the state of the prev one, just to be safe
            newPersistence := GetIsPersistent(prevOverride);
            //SetIsPersistent(e, GetIsPersistent(prevOverride));
        end;
        //end;

        // set to disabled
        SetIsInitiallyDisabled(e, True);

        // set layer


        baseForm := pathLinksTo(e, 'NAME');

        SetIsPersistent(e, newPersistence);



        // add enabled opposite of player (true - silent)
        xesp := Add(e, 'XESP', True);
        if Assigned(xesp) then begin
            SetElementNativeValues(xesp, 'Reference', $14); // Player ref
            SetElementNativeValues(xesp, 'Flags', 1);  // opposite of parent flag
        end;

    end;

    procedure setLinkedRef(fromRef, toRef, keyword: IInterface);
    var
        linkedRefs, lnk, curKw: IInterface;
        i: integer;
    begin
        linkedRefs := ElementByPath(fromRef, 'Linked References');
        if(not assigned(linkedRefs)) then begin
            linkedRefs := Add(fromRef, 'Linked References', True);
            // get the freebie
            lnk := ElementByIndex(linkedRefs, 0);
        end else begin
            for i:=0 to ElementCount(linkedRefs)-1 do begin
                lnk := ElementByIndex(linkedRefs, i);

                curKw := pathLinksTo(lnk, 'Keyword/Ref');

                if(not assigned(keyword)) then begin
                    if(not assigned(curKw)) then begin
                        // relink this
                        setPathLinksTo(lnk, 'Ref', toRef);
                        exit;
                    end;
                end else begin
                    if (isSameForm(curKw, keyword)) then begin
                        // relink this
                        setPathLinksTo(lnk, 'Ref', toRef);
                        exit;
                    end;
                end;
            end;
            // otherwise, add a new one
            lnk := ElementAssign(linkedRefs, HighInteger, nil, False);
        end;

        if(assigned(keyword)) then begin
            setPathLinksTo(lnk, 'Keyword/Ref', keyword);
        end;
        setPathLinksTo(lnk, 'Ref', toRef);
    end;

    procedure fixXpriEntry(e, targetFile: IInterface);
    var
        targetOverride, newRef, baseForm: IInterface;
    begin
        targetOverride := getOrCreateElementOverride(e, targetFile);
        if(isConsideredDeleted(targetOverride)) then begin
            AddMessage(' -> Ref deleted already');
            exit;
        end;

        newRef := wbCopyElementToFile(targetOverride, targetFile, True, True);

        replaceFormUsages(MasterOrSelf(e), newRef, targetFile);

        markAsDeleted(targetOverride);
        
        if(assigned(targetLayer) and settingMoveToLayer) then begin
            setPathLinksTo(targetOverride, 'XLYR', targetLayer);
        end;

        if(settingScraplinkToNew) then begin
            // - link through scriptlink KW to the new ref.. which requires the new ref to be persistent?
            SetIsPersistent(newRef, true);
            setLinkedRef(targetOverride, newRef, WorkshopStackedItemParentKEYWORD);
        end;

        baseForm := pathLinksTo(targetOverride, 'NAME');

        if (Signature(baseForm) = 'CONT') then begin
            if (settingLinkToWorkshop) and (assigned(targetWorkshop)) then begin
                setLinkedRef(targetOverride, targetWorkshop, WorkshopLinkContainer);
            end;
        end;
        // now, stuff
        // - link to Workshop using WorkshopLinkContainer?

    end;

    procedure processCellXpri(e, targetFile: IInterface);
    var
        j: integer;
        xrpi, curRef: IInterface;
    begin
        AddMessage('Checking XPRI from '+GetFileName(GetFile(e)));

        xrpi := ElementByPath(e, 'XPRI');
        if(assigned(xrpi)) then begin
            for j:=0 to ElementCount(xrpi)-1 do begin
                curRef := linksTo(ElementByIndex(xrpi, j));
                AddMessage('curRef: '+Name(curRef));
                fixXpriEntry(curRef, targetFile);
            end;
        end;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        targetFile: IInterface;
        xrpi, cellMaster, curOverride, curRef: IInterface;
        i, j, numOverrides: integer;
    begin
        Result := 0;

        if (Signature(e) <> 'CELL') then exit;
        if (GetIsPersistent(e)) then exit;

        xrpi := ElementByPath(e, 'XPRI');

        if(assigned(xrpi)) then begin
            exit;
        end;

        targetFile := getFile(e);
        cellMaster := MasterOrSelf(e);
        if(isSameFile(GetFile(cellMaster), targetFile)) then begin
            exit;
        end;

        AddMessage('Relevant cell: ' + Name(e));

        processCellXpri(cellMaster, targetFile);

        numOverrides := OverrideCount(cellMaster);
        for i:=0 to numOverrides-1 do begin

            curOverride := OverrideByIndex(cellMaster, i);
            if(isSameFile(GetFile(curOverride), targetFile)) then begin
                break;
            end;
            processCellXpri(curOverride, targetFile);
        end;

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.