{
    Run in a cell. You will be asked which layers to export.
}
unit ExportCellRefs;
    uses praUtil;

    var
        elemCache: TList;
        layerCache: TStringList;
        layerMap: TJsonObject;
        MousePosX, MousePosY: integer;
        layerList: TTreeView;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;


        layerCache := TStringList.create;
        elemCache := TList.create;
        layerMap := TJsonObject.create;
    end;

    function CreateSaveItemsFileDialog(title: string; filter: string = ''; doOverwrite: boolean): TSaveDialog;
    var
        objFile: TSaveDialog;
    begin
        objFile := TSaveDialog.Create(nil);
        Result := nil;

        objFile.Title := title;
        if(doOverwrite) then begin
            objFile.Options := objFile.Options + [ofOverwritePrompt];
        end;


        if(filter <> '') then begin
            objFile.Filter := filter;
            objFile.FilterIndex := 1;
        end;
        Result := objFile;
    end;

    function ShowSaveItemsFileDialog(title: string; doOverwrite: boolean): string;
    var
        objFile: TSaveDialog;
    begin
        objFile := CreateSaveItemsFileDialog(title, 'CSV Files|*.csv|All Files|*.*', doOverwrite);
        Result := '';
        try
            if objFile.Execute then begin
                Result := objFile.FileName;
            end;
        finally
            objFile.free;
        end;
    end;

    function getLayerDisplayName(jsonData: TJsonObject; isSelected: boolean): string;
    var
        numRefs: integer;
        actualName: string;
    begin
        actualName := jsonData['name'];
        numRefs := jsonData.A['refs'].count;
        if(numRefs > 0) then begin
            Result := actualName + ' (' + IntToStr(numRefs) + ')';
        end else begin
            Result := actualName;
        end;

        if(isSelected) then begin
            Result := '[x] '+Result;
        end else begin
            Result := '[_] '+Result;
        end;
    end;

    procedure selectAllChildren(layerData: TJsonObject; select: boolean);
    var
        i: integer;
        curName: string;
    begin
        //if(layerData.O['layers'].count == 0) then exit;

        for i:=0 to layerData.O['layers'].count-1 do begin
            curName := layerData.O['layers'].names[i];
            layerData.O['layers'].O[curName].B['selected'] := select;

            selectAllChildren(layerData.O['layers'].O[curName], select);
        end;
    end;

    procedure selectAllLayers(select: boolean);
    var
        i: integer;
        curName: string;
    begin
        for i:=0 to layerMap.count-1 do begin
            curName := layerMap.names[i];
            layerMap.O[curName].B['selected'] := select;

            selectAllChildren(layerMap.O[curName], select);
        end;
    end;

    procedure addLayers(layerList: TTreeView; jsonData: TJsonObject; parentNode: TTreeNode);
    var
        i, numRefs: integer;
        curName, displayName: string;
        curNode: TTreeNode;
        curSubData: TJsonObject;
    begin
        for i:=0 to jsonData.count-1 do begin
            curName := jsonData.Names[i];
            curSubData := jsonData.O[curName];
            {
            numRefs := curSubData.A['refs'].count;
            if(numRefs > 0) then begin
                displayName := curName + ' ('+IntToStr(numRefs)+')';
            end else begin
                displayName := curName;
            end;
            }
            displayName := getLayerDisplayName(curSubData, false);

            if(parentNode = nil) then begin
                curNode := layerList.Items.AddObject(nil, displayName, curSubData);
            end else begin
                curNode := layerList.Items.AddChildObject(parentNode, displayName, curSubData);
            end;

            curNode.ImageIndex := i;
            if(curSubData.O['layers'].count > 0) then begin
                addLayers(layerList, curSubData.O['layers'], curNode);
            end;
        end;
    end;

    function findComponentParentWindow(sender: TObject): TForm;
    begin
        Result := nil;

        if(sender.ClassName = 'TForm') then begin
            Result := sender;
            exit;
        end;

        if(sender.parent = nil) then begin
            exit;
        end;

        Result := findComponentParentWindow(sender.parent);
    end;

    procedure addAllChildLayers(targetList: TStringList; layerData: TJsonObject);
    var
        childLayers: TJsonArray;
        i: integer;
        curLayerName: string;
    begin
        curLayerName := layerData.S['name'];
        if(targetList.indexOf(curLayerName) >= 0) then exit;

        targetList.addObject(curLayerName, layerData);
        AddMessage('Added '+layerData.S['name']+' for exporting');
        childLayers := layerData.O['layers'];
        for i:=0 to childLayers.count-1 do begin
            curLayerName := childLayers.Names[i];
            addAllChildLayers(targetList, childLayers.O[curLayerName]);
        end;

    end;

    procedure processExporting(targetFileName: string; layerNames: TStringList; setLevelTo: string; doAppend: boolean);
    var
        mainList, lineList: TStringList;
        i, j, id: integer;
        layerName, scaleStr, posX, posY, posZ, rotX, rotY, rotZ, edid: string;
        layerData: TJsonObject;
        ref, base: IInterface;
    begin
        mainList := TStringList.create;
        if(doAppend and FileExists(targetFileName)) then begin
            mainList.LoadFromFile(targetFileName);
            mainList.add('');
        end else begin
            mainList.add('Form,Pos X,Pos Y,Pos Z,Rot X,Rot Y,Rot Z,Scale,iLevel,iStageNum,iStageEnd,iType,sVendorType,iVendorLevel,iOwnerNumber,sSpawnName,Requirements');
        end;

        // do refs
        for i:=0 to layerNames.count-1 do begin
            layerData := TJsonObject(layerNames.Objects[i]);
            //layerName := layerNames[i];
            AddMessage('Writing '+layerNames[i]);
            for j:=0 to layerData.A['refs'].count-1 do begin
                lineList := TStringList.create;
                id := layerData.A['refs'].I[j];
                ref := ObjectToElement(elemCache[id]);

                base := PathLinksTo(ref, 'NAME');
                edid := EditorID(base);
                scaleStr := GetElementEditValues(ref, 'XSCL');
                if(scaleStr = '') then begin
                    scaleStr := '1';
                end;

                posX := GetElementEditValues(ref, 'DATA\Position\X');
                posY := GetElementEditValues(ref, 'DATA\Position\Y');
                posZ := GetElementEditValues(ref, 'DATA\Position\Z');
                rotX := GetElementEditValues(ref, 'DATA\Rotation\X');
                rotY := GetElementEditValues(ref, 'DATA\Rotation\Y');
                rotZ := GetElementEditValues(ref, 'DATA\Rotation\Z');

                lineList.add(edid);
                lineList.add(posX);
                lineList.add(posY);
                lineList.add(posZ);
                lineList.add(rotX);
                lineList.add(rotY);
                lineList.add(rotZ);
                lineList.add(scaleStr);
                lineList.add(setLevelTo);
                lineList.add('');//stagenum
                lineList.add('');//stageend
                lineList.add('');//iType
                lineList.add('');//sVendorType
                lineList.add('');//iVendorLevel
                lineList.add('');//owner nr
                lineList.add('');//spawn name
                lineList.add('');//reqs

                lineList.Delimiter := ',';
                mainList.add(lineList.DelimitedText);

                lineList.free();
            end;
        end;

        AddMessage('Writing output file '+targetFileName);
        mainList.saveToFile(targetFileName);
        AddMessage('Finished.');

        mainList.free();
    end;

    procedure exportLayersHandler(Sender: TObject);
    var
        dialogResult, dialogCaption: string;
        layerList: TTreeView;
        frm: TForm;
        n: TTreeNode;
        i: integer;
        layerNames: TStringList;
        jData: TJsonObject;
        lvlInput: TEdit;
        appendToFile: TCheckBox;
        isAppendMode: boolean;
    begin

        frm := findComponentParentWindow(sender);
        layerList := TTreeView(frm.findComponent('layerList'));
        lvlInput := TEdit(frm.findComponent('lvlInput'));
        appendToFile := TCheckBox(frm.findComponent('appendToFile'));

        isAppendMode := appendToFile.checked;

        layerNames := TStringList.create;
        layerNames.Duplicates := dupIgnore;

        for i:=0 to layerList.Items.count-1 do begin
            n := layerList.Items.Item(i);
            jData := TJsonObject(n.Data);
            if(jData.B['selected']) then begin
                //AddMessage(n.Text + ' ' + jData.S['name']);
                //addAllChildLayers(layerNames, jData);
                layerNames.addObject(jData.S['name'], jData);
                AddMessage('Added '+jData.S['name']+' for exporting');
            end;
        end;

        if(layerNames.count = 0) then begin
            AddMessage('Nothing selected');
            layerNames.free();
            exit;
        end;

        if(isAppendMode) then begin
            dialogCaption := 'Select StageItemsSpawns to append';
        end else begin
            dialogCaption := 'Save StageItemSpawns file as';
        end;

        dialogResult := ShowSaveItemsFileDialog(dialogCaption, (not isAppendMode));
        if(dialogResult = '') then begin
            layerNames.free();
            exit;
        end;

        if(ExtractFileExt(dialogResult) = '') then begin
            dialogResult := dialogResult + '.csv';
        end;

        processExporting(dialogResult, layerNames, lvlInput.text, isAppendMode);

        layerNames.free();
    end;

    procedure layerListMouseDownHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
        MousePosX := X;
        MousePosY := Y;
    end;

    procedure redrawCheckboxes(layerList: TTreeView);
    var
        i: integer;
        n: TTreeNode;
        jData: TJsonObject;
        displayName: string;
    begin
        layerList.items.beginUpdate();
        for i:=0 to layerList.Items.count-1 do begin
            n := layerList.Items.Item(i);

            jData := TJsonObject(n.Data);

            displayName := getLayerDisplayName(jData, jData.B['selected']);
            n.text := displayName;
        end;
        layerList.items.endUpdate();
    end;

    procedure layerListClickHandler(sender: TObject);
    var
        layerList: TTreeView;

        n: TTreeNode;
        jData: TJsonObject;

        rect: TRect;
    begin
        layerList := TTreeView(sender);

        n := layerList.GetNodeAt(MousePosX, MousePosY);
        if(nil <> n) then begin
            rect := n.DisplayRect(true);

            if(
                (rect.Left <= MousePosX) and
                (rect.Top <= MousePosY) and
                (rect.Right >= MousePosX) and
                (rect.Bottom >= MousePosY)
            ) then begin
                jData := TJsonObject(n.Data);
                jData.B['selected'] := (not jData.B['selected']);
                selectAllChildren(jData, jData.B['selected']);

                redrawCheckboxes(layerList);
            end;
        end;
        // end try

    end;

    procedure menuSelectAllHandler(sender: TObject);
    begin
        selectAllLayers(true);
        redrawCheckboxes(layerList);
    end;

    procedure menuSelectNoneHandler(sender: TObject);
    begin
        selectAllLayers(false);
        redrawCheckboxes(layerList);
    end;


    procedure showExportGui();
    var
        frm: TForm;
        resultCode: cardinal;
        n: TTreeNode;
        selectLayersLabel, selectRefsLabel: TLabel;
        exportBtn, closeBtn: TButton;
        lvlInput: TEdit;
        appendToFile: TCheckBox;
        menu: TPopupMenu;
        selectAllItem, selectNoneItem: TMenuItem;
    begin
        frm := CreateDialog('Export References', 510, 350);
        layerList := TTreeView.create(frm);
        layerList.Name := 'layerList';
        layerList.Parent := frm;
        layerList.left := 10;
        layerList.top := 10;
        layerList.width := 340;
        layerList.height := 300;
        layerList.ReadOnly := True;
        layerList.onclick := layerListClickHandler;
        layerList.onMouseDown := layerListMouseDownHandler;

        // menu
        menu  := TPopupMenu.Create(layerList);
        menu.Name := 'menu';
        layerList.PopupMenu  := menu;
        //menu.onPopup := menuOpenHandler;
        selectAllItem := TMenuItem.create(menu);
        selectAllItem.Name := 'selectAllItem';
        selectAllItem.caption := 'Select All';
        selectAllItem.onclick := menuSelectAllHandler;
        menu.Items.add(selectAllItem);


        selectNoneItem := TMenuItem.create(menu);
        selectNoneItem.Name := 'selectNoneItem';
        selectNoneItem.caption := 'Select None';
        selectNoneItem.onclick := menuSelectNoneHandler;
        menu.Items.add(selectNoneItem);



        exportBtn := CreateButton(frm, 360, 10, 'Export Selected');

        CreateLabel(frm, 360, 50, 'Set Level To:');

        lvlInput := CreateInput(frm, 360, 70, '');
        lvlInput.Name := 'lvlInput';
        lvlInput.Text := '';

        appendToFile := CreateCheckbox(frm, 360, 100, 'Append to file');
        appendToFile.name := 'appendToFile';
        appendToFile.checked := true;

        closeBtn := CreateButton(frm, 360, 285, 'Close');

        exportBtn.width := 130;
        exportBtn.onclick := exportLayersHandler;
        closeBtn.width := 130;
        closeBtn.ModalResult := mrCancel;


        //layerList.RowSelect := True;
        //layerList.MultiSelect := True;
        // layerList.OnChange := TreeViewChange;

        addLayers(layerList, layerMap, nil);
        //layerListClickHandler(layerList);

        resultCode := frm.ShowModal();
    end;

    function getLayerObject(layerName: string; layerElem: IInterface): TJsonObject;
    var
        parentLayer: IInterface;
        parentObj: TJsonObject;
    begin
        if(not assigned(layerElem)) then begin
            Result := layerMap.O[layerName];
            Result.S['name'] := layerName;
            exit;
        end;

        parentLayer := pathLinksTo(layerElem, 'PNAM');
        if(not assigned(parentLayer)) then begin
            Result := layerMap.O[layerName];
            Result.S['name'] := layerName;
            exit;
        end;

        parentObj := getLayerObject(EditorID(parentLayer), parentLayer);
        Result := parentObj.O['layers'].O[layerName];
        Result.S['name'] := layerName;
    end;

    procedure processRef(e: IInterface);
    var
        layer, base: IInterface;
        layerName, sig: string;
        refIndex, layerIndex: integer;
        layerObj: TJsonObject;
    begin
        base := pathLinksTo(e, 'NAME');
        sig := Signature(base);
        if(sig = 'BNDS') or (sig = 'TXST') then begin
            AddMessage('Cannot process '+sig+', skipping '+FullPath(e));
            exit;
        end;
        // cannot: BNDS TXST
        //XLYR layer
        layer := pathLinksTo(e, 'XLYR');
        layerName := 'Default';
        if(assigned(layer)) then begin
            layerName := EditorID(layer);
        end;

        layerObj := getLayerObject(layerName, layer);
{
        layerIndex := layerCache.indexOf(layerName);
        if(layerIndex < 0) then begin
            if(assigned(layer)) then begin
                layerIndex := layerCache.add(layerName);
            end else begin
                layerIndex := layerCache.addObject(layerName, layer);
            end;
        end;
}
        refIndex := elemCache.add(e);
        layerObj.A['refs'].add(refIndex);
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        sig: string;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        // AddMessage('Processing: ' + FullPath(e));
        sig := Signature(e);
        if (sig = 'REFR') or (sig = 'ACHR') or (sig = 'PGRE') then begin
            processRef(e);
        end;

    end;

    function cloneJsonArrayAndSort(oldArr: TJsonArray): TJsonArray;
    var
        i, j, prevType: integer;
        curName: string;
        tempList: TStringList;
    begin
        Result := TJsonArray.create();
        for i:=0 to oldArr.count - 1 do begin
            prevType := oldArr.Types[i];
            case prevType of
                JSON_TYPE_STRING:
                    Result.add(oldArr.S[i]);
                JSON_TYPE_INT:
                    Result.add(oldArr.I[i]);
                JSON_TYPE_LONG:
                    Result.add(oldArr.L[i]);
                JSON_TYPE_ULONG:
                    Result.add(oldArr.U[i]);
                JSON_TYPE_FLOAT:
                    Result.add(oldArr.F[i]);
                JSON_TYPE_DATETIME:
                    Result.add(oldArr.D[i]);
                JSON_TYPE_BOOL:
                    Result.add(oldArr.B[i]=;
                JSON_TYPE_ARRAY:
                    Result.add(cloneJsonArrayAndSort(oldArr.A[i]));
                JSON_TYPE_OBJECT:
                    Result.add(sortJsonObject(oldArr.O[i]));
            end;
        end;
    end;

    function sortJsonObject(oldObj: TJsonObject): TJsonObject;
    var
        i, prevType: integer;
        curName: string;
        tempList: TStringList;
    begin
        tempList := TStringList.create();
        tempList.sorted := true;
        Result := TJsonObject.create();

        for i:=0 to oldObj.count-1 do begin
            curName := oldObj.Names[i];
            tempList.add(curName);
        end;


        for i:=0 to tempList.count-1 do begin
            curName := tempList[i];
            prevType := oldObj.Types[curName];
            case prevType of
                JSON_TYPE_STRING:
                    Result.S[curName] := oldObj.S[curName];
                JSON_TYPE_INT:
                    Result.I[curName] := oldObj.I[curName];
                JSON_TYPE_LONG:
                    Result.L[curName] := oldObj.L[curName];
                JSON_TYPE_ULONG:
                    Result.U[curName] := oldObj.U[curName];
                JSON_TYPE_FLOAT:
                    Result.F[curName] := oldObj.F[curName];
                JSON_TYPE_DATETIME:
                    Result.D[curName] := oldObj.D[curName];
                JSON_TYPE_BOOL:
                    Result.B[curName] := oldObj.B[curName];
                JSON_TYPE_ARRAY:
                    Result.A[curName] := cloneJsonArrayAndSort(oldObj.A[curName]);
                JSON_TYPE_OBJECT:
                    Result.O[curName] := sortJsonObject(oldObj.O[curName]);
            end;
        end;
        tempList.free();
    end;

    function Finalize: integer;
    var
        tmpJsonMap: TJsonObject;
    begin
        Result := 0;

        if(elemCache.count > 0) then begin
            tmpJsonMap := layerMap;
            layerMap := sortJsonObject(tmpJsonMap);
            tmpJsonMap.free();
            showExportGui();
        end;

        elemCache.free();
        layerMap.free();
        layerCache.free();

    end;

end.