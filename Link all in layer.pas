{
    Run on layer. It will ask you for a keyword and a target reference, then link everything to that target using that keyword.
    
    Keyword and Link Target can be editorIDs, formIDs, formId:filename, or that string you can copy out of xEdit
    
    "Unlinking Mode" means, it will remove links of all refs in the layer instead.
    "Unlink Target must match" means, it will only remove links going toward the Link Target.
}
unit scraplink;

    uses praUtil;
    const
        configFile = ScriptsPath + 'Link all in layer.cfg';

    var
        scraplinkKw, scraplinkTarget: IInterface;
        srcFile, destFile: IInterface;
        unLinkingMode, linkTargetMustMatch: boolean;

    procedure loadConfig();
    var
        i, j, breakPos: integer;
        curLine, curKey, curVal: string;
        lines : TStringList;
    begin
        // default
        scraplinkKw := nil;
        scraplinkTarget := nil;
        srcFile := nil;
        destFile := nil;
        unLinkingMode := false;
        linkTargetMustMatch := false;

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

                if(curKey = 'scraplinkKw') then begin
                    scraplinkKw := AbsStrToForm(curVal);
                end else if(curKey = 'scraplinkTarget') then begin
                    scraplinkTarget := AbsStrToForm(curVal);
                end else if(curKey = 'srcFile') then begin
                    srcFile := FindFile(curVal);
                end else if(curKey = 'destFile') then begin
                    destFile := FindFile(curVal);
                end else if(curKey = 'unLinkingMode') then begin
                    unLinkingMode := StrToBool(curVal);
                end else if(curKey = 'linkTargetMustMatch') then begin
                    linkTargetMustMatch := StrToBool(curVal);
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

        if(assigned(scraplinkKw)) then begin
            lines.add('scraplinkKw='+FormToAbsStr(scraplinkKw));
        end;
        if(assigned(scraplinkTarget)) then begin
            lines.add('scraplinkTarget='+FormToAbsStr(scraplinkTarget));
        end;
        if(assigned(srcFile)) then begin
            lines.add('srcFile='+GetFileName(srcFile));
        end;
        if(assigned(destFile)) then begin
            lines.add('destFile='+GetFileName(destFile));
        end;
        lines.add('unLinkingMode='+BoolToStr(unLinkingMode));
        lines.add('linkTargetMustMatch='+BoolToStr(linkTargetMustMatch));

        lines.saveToFile(configFile);
        lines.free();
    end;

    function showDialog(): boolean;
    var
        frm : TForm;
        resultCode: cardinal;
        inputKw, inputTarget: TEdit;
        yOffset: integer;
        btnCancel, btnOkay: TButton;
        unlinkCb, targetMustMatch: TCheckBox;
        srcFileMenu, targetFileMenu: TComboBox;
        i: integer;
        fileList: TSTringList;
        curFile: IInterface;
    begin
        fileList := TSTringList.create;
        for i := 0 to FileCount-1 do
        begin
            curFile := FileByIndex(i);
            fileList.AddObject(GetFileName(curFile), curFile);
        end;

        frm := CreateDialog('Config', 500, 300);
        yOffset := 0;
        CreateLabel(frm, 10, 10+yOffset, 'Link Keyword:');
        inputKw := CreateInput(frm, 100, 8+yOffset, '');
        if(assigned(scraplinkKw)) then begin
            inputKw.Text := FormToAbsStr(scraplinkKw);
        end;

        yOffset := yOffset + 30;
        CreateLabel(frm, 10, 10+yOffset, 'Link Target:');
        inputTarget := CreateInput(frm, 100, 8+yOffset, '');
        if(assigned(scraplinkTarget)) then begin
            inputTarget.Text := FormToAbsStr(scraplinkTarget);
        end;

        yOffset := yOffset + 40;

        inputKw.Width := 200;
        inputTarget.Width := 200;

        unlinkCb := CreateCheckbox(frm, 10, yOffset, 'Unlinking Mode');
        unlinkCb.checked := unLinkingMode;
        yOffset := yOffset + 20;
        targetMustMatch := CreateCheckbox(frm, 10, yOffset, 'Unlink Target must match');
        targetMustMatch.checked := linkTargetMustMatch;
        yOffset := yOffset + 20;


        CreateLabel(frm, 10, 10+yOffset, 'From file:');
        //srcFileMenu := CreateComboBox(frm, 10, 30+yOffset, 300, fileList);
        //targetFileSelector.Style := csDropDownList;
        srcFileMenu := CreateFileSelectDropdown(frm, 10, 30+yOffset, 300, srcFile, false);
        yOffset := yOffset + 50;
        CreateLabel(frm, 10, 10+yOffset, 'To file:');
        targetFileMenu := CreateFileSelectDropdown(frm, 10, 30+yOffset, 300, destFile, false);


        yOffset := yOffset + 70;
        btnOkay := CreateButton(frm, 10, yOffset, 'OK');
        btnOkay.ModalResult := mrYes;
        btnOkay.width := 75;

        btnCancel := CreateButton(frm, 90, yOffset, 'Cancel');
        btnCancel.ModalResult := mrCancel;
        btnCancel.width := 75;
        Result := false;

        resultCode := frm.ShowModal;
        if(resultCode = mrYes) then begin
            Result := true;
            scraplinkKw := findFormByString(inputKw.Text);
            scraplinkTarget := findFormOrRefByString(inputTarget.Text);
            unLinkingMode := unlinkCb.checked;
            linkTargetMustMatch := targetMustMatch.checked;
            
            srcFile := ObjectToElement(fileList.Objects[srcFileMenu.ItemIndex]);
            destFile := ObjectToElement(fileList.Objects[targetFileMenu.ItemIndex]);
            
            if(not assigned(scraplinkKw)) then begin
                AddMessage('Failed to find "'+inputKw.Text+'" for link keyword');
                Result := false;
            end;
            
            if(unLinkingMode) then begin
                if(linkTargetMustMatch) then begin
                    if(not assigned(scraplinkTarget)) then begin
                        AddMessage('Failed to find "'+inputTarget.Text+'" for linking target');
                        Result := false;
                    end;
                end;
            end else begin
                if(not assigned(scraplinkTarget)) then begin
                    AddMessage('Failed to find "'+inputTarget.Text+'" for linking target');
                    Result := false;
                end;
            end;
        end;

        fileList.free();
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        loadConfig();
        if(not showDialog()) then begin
            AddMessage('cancelled');
            Result := 1;
            exit;
        end;
        Result := 0;
{
        unLinkingMode := true;

        scraplinkKw := FindObjectByEdid('WorkshopItemKeyword');
        //scraplinkKw := nil;
        scraplinkTarget := getFormByLoadOrderFormID($01000FB5);

        linkTargetMustMatch := true;
}
        saveConfig();
    end;

    function isScrappable(e: IInterface): boolean;
    var
        i: integer;
        curRef, product: IInterface;
    begin
        Result := false;
        for i:=0 to ReferencedByCount(e)-1 do begin
            curRef := ReferencedByIndex(e, i);

            if(Signature(curRef) = 'COBJ') then begin
                product := pathLinksTo(curRef, 'CNAM');

                if(isSameForm(product, e)) then begin
                    Result := true;
                    exit;
                end;
            end else if(Signature(curRef) = 'FLST') then begin
                if(isScrappable(curRef)) then begin
                    Result := true;
                    exit;
                end;
            end;
        end;

    end;

    function isSomehowDeleted(e: IInterface): boolean;
    begin
        Result := (GetIsDeleted(e) or GetIsInitiallyDisabled(e));
    end;

    procedure processRefUnlink(ref: IInterface);
    var
        i, elemCount: integer;
        linkedRefs, curEntry, curKw, curTarget: IInterface;
    begin
        linkedRefs := ElementByPath(ref, 'Linked References');
        if(not assigned(linkedRefs)) then exit;

        elemCount := ElementCount(linkedRefs);
        for i:=0 to elemCount-1 do begin
            curEntry := ElementByIndex(linkedRefs, i);
            curKw := PathLinksTo(curEntry, 'Keyword/Ref');
//linkTargetMustMatch
            if(isSameForm(curKw, scraplinkKw)) then begin
                curTarget := PathLinksTo(curEntry, 'Ref');
                if(linkTargetMustMatch and not Equals(curTarget, scraplinkTarget)) then begin
                    continue;
                end;
                // found for unlinkage
                if(elemCount = 1) then begin
                    // delete the entire linkedRefs
                    Remove(linkedRefs);
                end else begin
                    // delete curEntry
                    // try
                    RemoveElement(linkedRefs, curEntry);
                end;
                exit;
            end;
        end;

    end;

    procedure processRef(ref: IInterface);
    var
        i: integer;
        linkedRefs, testEntry, curEntry, curKw: IInterface;
    begin
        linkedRefs := ElementByPath(ref, 'Linked References');
        curEntry := nil;
        if(assigned(linkedRefs)) then begin
            for i:=0 to ElementCount(linkedRefs)-1 do begin
                testEntry := ElementByIndex(linkedRefs, i);
                curKw := PathLinksTo(testEntry, 'Keyword/Ref');

                if(isSameForm(curKw, scraplinkKw)) then begin
                    // AddMessage('Skipping because scraplinked already: ' + Name(ref));
                    exit;
                end;
            end;
        end else begin
            linkedRefs := Add(ref, 'Linked References', true);
            if(ElementCount(linkedRefs) > 0) then begin
                curEntry := ElementByIndex(linkedRefs, 0);
            end;
        end;

        AddMessage('LINKING: ' + Name(ref));

        if(not assigned(curEntry)) then begin
            curEntry := ElementAssign(linkedRefs, HighInteger, nil, false);
        end;

        SetPathLinksTo(curEntry, 'Keyword/Ref', scraplinkKw);
        SetPathLinksTo(curEntry, 'Ref', scraplinkTarget);
    end;


    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        i: integer;
        baseObj, curRef: IInterface;

    begin
        Result := 0;

        if(Signature(e) <> 'LAYR') then exit;


        // comment this out if you don't want those messages

        for i:=0 to ReferencedByCount(e)-1 do begin
            curRef := ReferencedByIndex(e, i);
            if(not IsSameFile(GetFile(curRef), srcFile)) then continue;
            
            if(isReferenceSignature(signature(curRef))) then begin
                {
                if(GetIsPersistent(curRef)) then begin
                    AddMessage('Skipping because persistent: '+Name(curRef));
                    continue;
                end;
                }

                if(isSomehowDeleted(curRef)) then begin
                    continue;
                end;

                baseObj := PathLinksTo(curRef, 'NAME');

                // try linking literally EVERYTHING
                {
                if(signature(baseObj) <> 'STAT') and (signature(baseObj) <> 'SCOL') then begin
                    //AddMessage('Skipping because not static: '+Name(curRef));
                    continue;
                end;

                if(isScrappable(baseObj)) then begin
                    //AddMessage('Skipping because scrappable: '+Name(curRef));
                    continue;
                end;
                }
                
                if(not IsSameFile(GetFile(curRef), destFile)) then begin
                    curRef := getOrCreateElementOverride(curRef, destFile);
                end;
                
                
                if(unLinkingMode) then begin
                    processRefUnlink(curRef);
                end else begin
                    processRef(curRef);
                end;
            end;
        end;

        // processing code goes here

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.