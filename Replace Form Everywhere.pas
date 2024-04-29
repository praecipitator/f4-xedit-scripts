{
    Use replace-map.txt to define replacements, one replacement per line, like this:
    oldFormId=newFormId
    Run on something within the target file, will replace any occurence within that file.
}
unit ReplaceFormEverywhere;

    uses praUtil;
    const
        replaceMapFile = ScriptsPath + 'replace-map.txt';
        createIfMissing = false;
    var
        StrReplace: string;

        fileToDo, formSearch, formReplace: IInterface;

        replaceMap: TStringList;
        replaceMapVals: TStringList;
        
    

    function registerReplacement(key: String; val: String): boolean;
    var
        i :Integer;
        objSearch, curObj:IInterface;
        // obj: TObject;
    begin
        Result := true;
        objSearch := findObjectByEdid(key);
        if(not assigned(objSearch)) then begin
            AddMessage('WARN: can''t find object for search key '+key+', skipping');
            exit;
        end;
        curObj := findObjectByEdid(val);
        if(not assigned(curObj)) then begin
            if(createIfMissing) then begin
                curObj := wbCopyElementToFile(objSearch, fileToDo, true, true);
                SetElementEditValues(curObj, 'EDID', val);
            end else begin;
                AddMessage('ERROR: found no element with ID '+val);
                //halt;
                Result := false;
                exit;
            end;
        end;

        i := replaceMap.indexOf(key);
        if i < 0 then begin
            i := replaceMap.add(key);
            //replaceMapVals.insert(i, val);
            replaceMapVals.insertObject(i, val, curObj);

        end else begin
            //replaceMapVals[i] := val;
            replaceMapVals[i] := val;
            replaceMapVals.Objects[i] := curObj;
        end;
    end;

    function getReplacementElem(key: String): IInterface;
    var
        i: Integer;
        obj: TObject;
    begin
        result := nil;
        i := replaceMap.indexOf(key);
        if(i >= 0) then begin
            obj := replaceMapVals.objects[i];
            Result := ObjectToElement(obj);
        end;
    end;

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

        list := TStringList.Create;
        list.loadFromFile( replaceMapFile);
        for i:=0 to list.count-1 do begin
            curLine := trim(list[i]);
            if (length(curLine) > 0) then begin

                if (curLine[1] <> ';') then begin
                    spacePos := -1;
                    for j:=1 to length(curLine) do begin
                        if curLine[j] = '=' then begin
                            spacePos := j;
                            break;
                        end;
                        //AddMessage(curLine[j]);
                    end;
                    if spacePos <> -1 then begin
                        key := trim(copy(curLine, 0, spacePos-1));
                        val := trim(copy(curLine, spacePos+1, length(curLine)));
                        if (key <> '') and (val <> '') then begin
                            if(not registerReplacement(key, val)) then begin
                                Result := 1;
                                exit;
                            end;
                        end;
                    end;
                end;
            end;
        end;
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
                AddMessage('Found one');
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
        searchEdid: string;
    begin
        Result := 0;
        if(not assigned(fileToDo)) then begin
            exit;
        end;

        for j := 0 to replaceMap.count-1 do begin
            searchEdid := replaceMap[j];
            AddMessage('== PROCESSING '+searchEdid+' ==');
            formSearch := FindObjectByEdid(searchEdid);
            formReplace := ObjectToElement(replaceMapVals.objects[j]);

            for i := ReferencedByCount(formSearch)-1 downto 0 do begin
                curRef := ReferencedByIndex(formSearch, i);
                if(GetFileName(GetFile(curRef)) = GetFileName(fileToDo)) then begin
                    //dumpElem(curRef);
                    
                    replaceFormIn(curRef, formSearch, formReplace);
                end;
            end;
        end;
    end;

end.
