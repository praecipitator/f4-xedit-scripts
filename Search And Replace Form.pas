{
    Run on the file you want to do the replacement in. You will be asked for stuff.
}
unit ReplaceFormEverywhere;

uses praUtil;

var
    StrReplace: string;

    fileToDo, formSearch, formReplace: IInterface;

    replaceMap: TStringList;
    replaceMapVals: TStringList;



    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    var
        list: TStringList;
        curLine: String;
        i, j: Integer;
        endPos: Integer;
        key: String;
        val: String;
        spacePos: Integer;
    begin
        Result := 0;
        replaceMap     := TStringList.create;
        replaceMapVals := TStringList.create;

    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        fileToDo := GetFile(e);

    end;

    procedure replaceFormIn(container, search, repl: IInterface);
    var
        i: integer;
        child, lt: IInterface;
    begin
        lt := LinksTo(container);
        if(assigned(lt)) then begin
            if(IsSameForm(lt, search)) then begin
                AddMessage('Replacing in:');
                AddMessage('  '+FullPath(container));
                setLinksTo(container, repl);
            end;
        end;

        for i := 0 to ElementCount(container)-1 do begin

            child := ElementByIndex(container, i);
            // AddMessage(prefix+DisplayName(child)+'='+GetEditValue(child));
            replaceFormIn(child, search, repl);
        end;
    end;


    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        i, j, numRefs: cardinal;
        curRef, formSearch: IInterface;
        searchEdid, replaceEdid: string;
        frm: TForm;
        inputSearch, inputReplace: TEdit;
        btnOk, btnCancel: TButton;
        resultCode: cardinal;
    begin
        Result := 0;
        if(not assigned(fileToDo)) then begin
            exit;
        end;

        frm := CreateDialog('Search and replace form', 400, 180);
        CreateLabel(frm, 10, 2, 'Input Editor or Form IDs to search and replace forms');
        CreateLabel(frm, 10, 18, 'Selected file: '+GetFileName(fileToDo));


        CreateLabel(frm, 10, 50, 'Search');
        inputSearch := CreateInput(frm, 80, 47, '');
        inputSearch.width := 250;

        CreateLabel(frm, 10, 75, 'Replace with');
        inputReplace := CreateInput(frm, 80, 72, '');
        inputReplace.width := 250;


        btnOk := CreateButton(frm, 50, 120, 'OK');
        btnOk.ModalResult := mrYes;
        btnOk.Default := true;
        btnOk.width := 90;

        btnCancel := CreateButton(frm, 250, 120, 'Cancel');
        btnCancel.ModalResult := mrCancel;
        btnCancel.width := 90;

        resultCode := frm.ShowModal;

        if(resultCode <> mrYes) then begin
            AddMessage('Cancelled');
            exit;
        end;

        searchEdid := trim(inputSearch.text);
        replaceEdid :=trim(inputReplace.text);

        if(searchEdid = '') or (replaceEdid = '') then begin
            AddMessage('Both fields must be filled out');
            exit;
        end;

        formSearch  := findFormByString(searchEdid);
        if(not assigned(formSearch)) then begin
            AddMessage('Could not find '+searchEdid);
            exit;
        end;

        formReplace := findFormByString(replaceEdid);
        if(not assigned(formReplace)) then begin
            AddMessage('Could not find '+replaceEdid);
            exit;
        end;




        for i := ReferencedByCount(formSearch)-1 downto 0 do begin
            curRef := ReferencedByIndex(formSearch, i);
            if(GetFileName(GetFile(curRef)) = GetFileName(fileToDo)) then begin
                //dumpElem(curRef);

                replaceFormIn(curRef, formSearch, formReplace);
            end;
        end;

{

}
    end;

end.