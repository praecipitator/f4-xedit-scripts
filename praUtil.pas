{
    Some useful functions.

    Version is now in PRA_UTIL_VERSION
}
unit PraUtil;
    const
        // the version constant
        PRA_UTIL_VERSION = 16.1;


        // file flags
        FILE_FLAG_ESM       = 1;
        FILE_FLAG_LOCALIZED = 128;
        FILE_FLAG_ESL       = 512;
        FILE_FLAG_IGNORED   = 4096;

        // xedit version constants
        XEDIT_VERSION_404 = $04000400;
        XEDIT_VERSION_415h = $04010508;
        XEDIT_VERSION_415j = $0401050A;

        // JSON constants
        JSON_TYPE_NONE      = jdtNone; // none
        JSON_TYPE_STRING    = jdtString; // string
        JSON_TYPE_INT       = jdtInt; // int
        JSON_TYPE_LONG      = jdtLong; // long
        JSON_TYPE_ULONG     = jdtULong; // ulong
        JSON_TYPE_FLOAT     = jdtFloat; // float
        JSON_TYPE_DATETIME  = jdtDateTime; // datetime
        JSON_TYPE_BOOL      = jdtBool; // bool
        JSON_TYPE_ARRAY     = jdtArray; // array
        JSON_TYPE_OBJECT    = jdtObject; // object

        // misc constants
        MAX_EDID_LENGTH = 87; // 99-12, because 12 is the length of DUPLICATE000. And no, I don't care that the NG supposedly fixed this.
        STRING_LINE_BREAK = #13#10;



    // xEdit stuff
    {
        Builds an xEdit version cardinal out of individual parts.
        "Build" must be a lowercase letter, a-z, or an empty string if no build letter is present. Or a number 0-255, I guess.
    }
    function xEditVersionToCardinal(major, minor, release: Cardinal; Build: string): Cardinal;
    var
        buildOrd, buildLength: integer;
        buildChar: char;

    begin
        Result :=
            ((major   and $000000FF) shl 24) or
            ((minor   and $000000FF) shl 16) or
            ((release and $000000FF) shl  8);


        buildLength := Length(Build);
        if(buildLength >= 1) then begin
            if(buildLength = 1) then begin
                // try parsing it as a letter, a-z
                buildChar := Copy(Build, 1, 1);

                buildOrd := Ord(buildChar);

                // ord(a)=97
                // ord(z)=122

                if(buildOrd >= 97) and (buildOrd <= 122) then begin
                    Result := Result + buildOrd - 96;
                    exit;
                end;
            end;

            // if still alive, try to parse Build as a number
            buildOrd := IntToStr(Build);

            Result := Result + buildOrd and $FF;
        end;
    end;

    {
        Decodes an xEdit version cardinal, like wbVersionNumber, into a string.
    }
    function xEditVersionCardinalToString(versionCardinal: cardinal): string;
    var
        major, minor, release, build, buildOrd: integer;
        buildString: string;
    begin
        major   := (versionCardinal and $FF000000) shr 24;
        minor   := (versionCardinal and   $FF0000) shr 16;
        release := (versionCardinal and     $FF00) shr 8;
        build   := (versionCardinal and       $FF);

        Result := IntToStr(major) + '.' + IntToStr(minor) + '.' + IntToStr(release);

        if(build > 0) then begin
            if(build <= 25) then begin
                buildOrd := build - 1 + 97;
                Result := Result + Chr(buildOrd);
            end else begin
                Result := Result + '.' + IntToStr(build);
            end;
        end;
    end;

    // generic stuff
    {
        Check if two file variables are referring to the same file
    }
    function FilesEqual(file1, file2: IwbFile): boolean;
    begin
        // Should be faster than comparing the filenames
        Result := (GetLoadOrder(file1) = GetLoadOrder(file2));
    end;

    function isSameFile(file1, file2: IwbFile): boolean;
    begin
        Result := FilesEqual(file1, file2);
    end;

    {
        Returns if e is either deleted, or flagged as initially disabled and opposite to player
    }
    function isConsideredDeleted(e: IInterface): boolean;
    var
        xesp: IInterface;
        flags: integer;
    begin
        Result := true;
        if(GetIsDeleted(e)) then exit;

        if(GetIsInitiallyDisabled(e)) then begin
            xesp := pathLinksTo(e, 'XESP\Reference');
            if(FormID(xesp) = 20) then begin
                flags := GetElementNativeValues(e, 'XESP\Flags');
                // yes player
                if((flags or 1) <> 0) then begin
                    //AddMessage('the flags are '+IntToStr(flags));
                    exit;
                end;
            end;

        end;
        Result := false;
    end;

    {
        Returns if e is scrappable, either directly or via a scrap FLST, recursively
    }
    function isBaseObjectScrappable(e: IInterface): boolean;
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
                if(isBaseObjectScrappable(curRef)) then begin
                    Result := true;
                    exit;
                end;
            end;
        end;

    end;

    {
        Check if two IInterfaces are equivalent, by recursively comparing the edit values of their contents
    }
    function ElementsEquivalent(e1, e2: IInterface): boolean;
    var
        i, count1, count2: Integer;
        child1, child2: IInterface;
        key1, key2, val1, val2: string;
        tmpResult: boolean;
    begin
        Result := false;

        // trivial crap
        if (not assigned(e1)) and (not assigned(e2)) then begin
            Result := true;
            exit;
        end;

        if(Equals(e1, e2)) then begin
            Result := true;
            exit;
        end;

        count1 := ElementCount(e1);
        count2 := ElementCount(e2);

        if(count1 <> count2) then exit;

        if(count1 = 0) then begin
            val1 := GetEditValue(e1);
            val2 := GetEditValue(e2);
            Result := (val1 = val2);
            exit;
        end;

        for i := 0 to ElementCount(e1)-1 do begin
            child1 := ElementByIndex(e1, i);
            child2 := ElementByIndex(e2, i);

            key1 := DisplayName(child1);
            key2 := DisplayName(child2);

            if(key1 <> key2) then exit;

            if (key1 <> '') then begin
                tmpResult := ElementsEquivalent(child1, child2);
                if(not tmpResult) then exit;
            end;
        end;

        Result := true;
    end;

    {
        Gets a file object by filename
    }
    function FindFile (name: String): IwbFile;
    var
        i: integer;
        curFile: IwbFile;
    begin
        name := LowerCase(name);
        Result := nil;
        for i := 0 to FileCount-1 do
        begin
            curFile := FileByIndex(i);
            if(LowerCase(GetFileName(curFile)) = name) then begin
                Result := curFile;
                exit;
            end;
        end;
    end;

    {
        Returns whenever a file has the ESL header
    }
    function isFileLight(f: IInterface): boolean;
    begin
        // Turns out, xEdit had this thing all along. Good thing we have such a good documentation for it /s
        Result := GetIsESL(f);
    end;

    {
        Gets and trims element edit values, for SimSettlements city plans
    }
    function geevt(e: IInterface; name: string): string;
    begin
        Result := trim(GetElementEditValues(e, name));
    end;

    {
        Gets an object by editor ID from any currently loaded file and any group.
    }
    function FindObjectByEdid(edid: String): IInterface;
    var
        iFiles: integer;
        curFile: IInterface;
        curRecord: IInterface;
    begin
        Result := nil;

        if(edid = '') then exit;

        curRecord := nil;
        for iFiles := 0 to FileCount-1 do begin
            curFile := FileByIndex(iFiles);

            if(assigned(curFile)) then begin

                curRecord := FindObjectInFileByEdid(curFile, edid);
                if (assigned(curRecord)) then begin
                    Result := curRecord;
                    exit;
                end;
            end;
        end;
    end;

    {
        Gets an object by editor ID from the given file and any group.
    }
    function FindObjectInFileByEdid(theFile: IInterface; edid: string): IInterface;
    var
        iSigs: integer;
        curGroup: IInterface;
        curRecord: IInterface;
    begin
        Result := nil;

        if(edid = '') then exit;

        curRecord := nil;
        for iSigs:=0 to ElementCount(theFile)-1 do begin
            curGroup := ElementByIndex(theFile, iSigs);
            if (Signature(curGroup) = 'GRUP') then begin
                curRecord := MainRecordByEditorID(curGroup, edid);
                if(assigned(curRecord)) then begin
                    Result := curRecord;
                    exit;
                end;
            end;
        end;
    end;

    {
        Gets an object by editor ID from any currently loaded file and any group.
        This is not a performant function.
    }
    function FindObjectByEdidAndSignature(edid, sig: String): IInterface;
    var
        iFiles: integer;
        curFile: IInterface;
        curRecord: IInterface;
    begin
        Result := nil;

        if(edid = '') then exit;

        curRecord := nil;
        for iFiles := 0 to FileCount-1 do begin
            curFile := FileByIndex(iFiles);

            if(assigned(curFile)) then begin

                curRecord := FindObjectInFileByEdidAndSignature(curFile, edid, sig);
                if (assigned(curRecord)) then begin
                    Result := curRecord;
                    exit;
                end;
            end;
        end;
    end;

    {
        Gets an object by editor ID and signature from the given file and any group.
    }
    function FindObjectInFileByEdidAndSignature(theFile: IInterface; edid, sig: string): IInterface;
    var
        curGroup: IInterface;
    begin
        Result := nil;

        if (edid = '') or (sig = '') then exit;

        curGroup := GroupBySignature(theFile, sig);
        if(assigned(curGroup)) then begin
            Result := MainRecordByEditorID(curGroup, edid);
        end;
    end;

    {
        Tries to find a reference with a given editor id. Iterates all the cells. Probably even worse than FindObjectByEdid
    }
    function FindReferenceByEdid(edid: string): IInterface;
    var
        iFiles: integer;
        curFile: IInterface;
        curRecord: IInterface;
    begin
        Result := nil;

        if(edid = '') then exit;

        curRecord := nil;
        for iFiles := 0 to FileCount-1 do begin
            curFile := FileByIndex(iFiles);

            if(assigned(curFile)) then begin

                curRecord := FindReferenceInFileByEdid(curFile, edid);
                if (assigned(curRecord)) then begin
                    Result := curRecord;
                    exit;
                end;
            end;
        end;
    end;

    function FindReferenceInBlockGrpByEdid(blockGrp: IInterface; edid: string; checkPersistentCell: boolean): IInterface;
    var
        iWorlds, blockidx, subblockidx, cellidx: integer;
        worlds, interiors, wrldgrup, block, subblock, cell, wrld: IInterface;
        foo: IInterface;
        curSig: string;
    begin
        // traverse Blocks
        for blockidx := 0 to ElementCount(blockGrp)-1 do begin
            block := ElementByIndex(blockGrp, blockidx);

            // traverse SubBlocks
            for subblockidx := 0 to ElementCount(block)-1 do begin
                subblock := ElementByIndex(block, subblockidx);
                if(Signature(subblock) <> 'GRUP') then begin
                    continue;
                end;
                // traverse Cells
                for cellidx := 0 to ElementCount(subblock)-1 do begin
                    cell := ElementByIndex(subblock, cellidx);
                    curSig := Signature(cell);

                    if (curSig = 'CELL') then begin
                        Result := findNamedReference(cell, edid);
                        if (assigned(Result)) then exit;
                    end else if(isReferenceSignature(curSig)) then begin
                        // this happens for persistent cells...
                        if(EditorID(cell) = edid) then begin
                            Result := cell;
                            exit;
                        end;
                    end;
                end;
            end;
        {
        }
        end;
    end;

    function FindReferenceInFileByEdid(theFile: IInterface; edid: string): IInterface;
    var
        iWorlds, blockidx, subblockidx, cellidx: integer;
        worlds, interiors, wrldgrup, block, subblock, cell, wrld: IInterface;

    begin
        // interiors
        interiors := GroupBySignature(theFile, 'CELL');
        Result := FindReferenceInBlockGrpByEdid(interiors, edid, false);
        if(assigned(Result)) then exit;

        // exteriors
        worlds := GroupBySignature(theFile, 'WRLD');
        for iWorlds:=0 to ElementCount(worlds)-1 do begin
            wrld := ElementByIndex(worlds, iWorlds);
            // need to find this world's persistent cell
            wrldgrup := ChildGroup(wrld);
            Result := FindReferenceInBlockGrpByEdid(wrldgrup, edid, true);
            if(assigned(Result)) then exit;
        end;
    end;


    function GetFormByEdid(edid: string): IInterface;
    begin
        Result := FindObjectByEdid(edid);
    end;

    function findInteriorCellByEdid(edid: string): IInterface;
    var
        iFiles: integer;
        curFile: IInterface;
        curRecord: IInterface;
    begin
        Result := nil;

        if(edid = '') then exit;

        curRecord := nil;
        for iFiles := 0 to FileCount-1 do begin
            curFile := FileByIndex(iFiles);

            if(assigned(curFile)) then begin

                curRecord := findInteriorCellInFileByEdid(curFile, edid);
                if (assigned(curRecord)) then begin
                    Result := curRecord;
                    exit;
                end;
            end;
        end;
    end;

    {
        Iterates through all interior cells in a file, and returns the one with the matching edid
    }
    function findInteriorCellInFileByEdid(sourceFile: IInterface; edid: String): IInterface;
    var
        cellGroup: IInterface;
        block, subblock, cell: IInterface;
        i, j, k: integer;
    begin
        Result := nil;
        cellGroup := GroupBySignature(sourceFile, 'CELL');

        for i:=0 to ElementCount(cellGroup)-1 do begin
            block := ElementByIndex(cellGroup, i);

            for j:=0 to ElementCount(block)-1 do begin
                subblock := ElementByIndex(block, j);

                for k:=0 to ElementCount(subblock)-1 do begin
                    cell := ElementByIndex(subblock, k);

                    if(Signature(cell) = 'CELL') then begin
                        if(EditorID(cell) = edid) then begin
                            Result := cell;
                            exit;
                        end;
                    end;
                end;
            end;
        end;
    end;

    {
        Searches in persistent and temporary references for one with a specific editorID
    }
    function findNamedReference(cell: IInterface; refEdid: string): IInterface;
    var
        i: integer;
        cur, test: IInterface;
    begin
        //  persistent
        test := FindChildGroup(ChildGroup(cell), 8, cell);
        for i:=0 to ElementCount(test)-1 do begin
            cur := ElementByIndex(test, i);
            if(EditorID(cur) = refEdid) then begin
                Result: = cur;
                exit;
            end;
        end;

        // temporary
        test := FindChildGroup(ChildGroup(cell), 9, cell);
        for i:=0 to ElementCount(test)-1 do begin
            cur := ElementByIndex(test, i);
            if(EditorID(cur) = refEdid) then begin
                Result: = cur;
                exit;
            end;
        end;
    end;

    {
        Searches in subject using regexString. Returns the matched group of the given number.
        Matched groups begin at 1, with 0 being the entire matched string.
        Returns empty string on failure.

        Example:
        regexExtract('123 foobar 235 what', '([0-9]+) what', 1) -> '235'

    }
    function regexExtract(subject, regexString: string; returnMatchNr: integer): string;
    var
        regex: TPerlRegEx;
    begin
        regex := TPerlRegEx.Create();
        Result := '';
        try
            regex.RegEx := regexString;
            regex.Subject := subject;

            if(regex.Match()) then begin
                // misnomer, is actually the highest valid index of regex.Groups
                if(regex.GroupCount >= returnMatchNr) then begin
                    Result := regex.Groups[returnMatchNr];
                end;
            end;
        finally
            RegEx.Free;
        end;
    end;

    function regexReplace(subject, regexString, replacement: string): string;
    var
        regex: TPerlRegEx;
    begin
        Result := '';
        regex  := TPerlRegEx.Create();
        try
            regex.RegEx := regexString;
            regex.Subject := subject;
            regex.Replacement := replacement;
            regex.ReplaceAll();
            Result := regex.Subject;
        finally
            RegEx.Free;
        end;
    end;

    {
        Tries to extract the FormID from a string like 'REObjectJS01Note "Note" [BOOK:00031901]'.
        If the string is just a plain hex number already, should parse that as well.
    }
    function findFormIdInString(someStr: string): cardinal;
    var
        regex: TPerlRegEx;
        maybeFormId : cardinal;
        maybeMatch: string;
    begin
        maybeFormId := 0;
        Result := 0;
        if (someStr = '') then exit;

        maybeMatch := regexExtract(someStr, '^([0-9a-fA-F]+)$', 1);
        if (maybeMatch <> '') then begin
            maybeFormId := StrToInt64('$' + maybeMatch);
            if(maybeFormId > 0) then begin
                Result := maybeFormId;
                exit;
            end;
        end;

        maybeMatch := regexExtract(someStr, '\[....:([0-9a-fA-F]{8})\]', 1);
        if (maybeMatch <> '') then begin
            maybeFormId := StrToInt64('$' + maybeMatch);
            if(maybeFormId > 0) then begin
                Result := maybeFormId;
                exit;
            end;
        end;
    end;

    {
        Tries to find a form by strings like:
            - Fallout4.esm:00031901
            - REObjectJS01Note "Note" [BOOK:00031901]
            - 00031901
            - REObjectJS01Note
        In the first case, it only cares about the FormID, not the EditorID, if any.
    }
    function findFormByString(someStr: string): IInterface;
    var
        maybeFormId: cardinal;
    begin
        Result := AbsStrToForm(someStr);
        if(assigned(Result)) then begin
            exit;
        end;
        maybeFormId := findFormIdInString(someStr);
        if(maybeFormId > 0) then begin
            Result := getFormByLoadOrderFormID(maybeFormId);
            // return nil here if it failed, too
            exit;
        end;

        Result := FindObjectByEdid(someStr);
    end;

    {
        Like above, but also tries to find a reference
    }
    function findFormOrRefByString(someStr: string): IInterface;
    var
        maybeFormId: cardinal;
    begin
        Result := AbsStrToForm(someStr);
        if(assigned(Result)) then begin
            exit;
        end;
        maybeFormId := findFormIdInString(someStr);
        if(maybeFormId > 0) then begin
            Result := getFormByLoadOrderFormID(maybeFormId);
            // return nil here if it failed, too
            exit;
        end;

        Result := FindObjectByEdid(someStr);
        if(not assigned(Result)) then begin
            Result := FindReferenceByEdid(someStr);
        end;
    end;


    {
        Go upwards from a child to a main record
    }
    function getParentRecord(child: IInterface): IInterface;
    var
        t1: TwbElementType;
        t2: TwbDefType;
    begin
        Result := nil;
        t1 := ElementType(child);

        if(t1 = etMainRecord) then begin
            Result := child;
            exit;
        end;

        Result := getParentRecord(GetContainer(child));
    end;

    function getFormByFilenameAndFormID(filename: string; id: cardinal): IInterface;
    var
        fileObj: IInterface;
        {localFormId: cardinal;}
    begin

        Result := nil;
        fileObj := FindFile(filename);
        if(not assigned(fileObj)) then begin
            exit;
        end;
        Result := getFormByFileAndFormID(FindFile(filename), id);
        {
        localFormId := FileToLoadOrderFormID(fileObj, id);

        Result := RecordByFormID(fileObj, localFormId, true);
        }
    end;

    procedure loadMasterList(list: TStringList; theFile: IInterface);
    var
		curFile: IInterface;
		curFileName: string;
		i: integer;
	begin
		for i:=0 to MasterCount(theFile)-1 do begin
			curFile := MasterByIndex(theFile, i);
			curFileName := GetFileName(curFile);

            if(list.indexOf(curFileName) < 0) then begin
                list.addObject(curFileName, curFile);
                loadMasterList(list, curFile);
            end;
		end;
    end;


	function getMasterList(theFile: IInterface): TStringList;
	begin
		Result := TStringList.create();

        loadMasterList(Result, theFile);
	end;

    function GetFirstNonOverrideElement(theFile: IwbFile): IInterface;
    var
        curGroup, curRecord: IInterface;
        iSigs, i: integer;
    begin
        for iSigs:=0 to ElementCount(theFile)-1 do begin
            curGroup := ElementByIndex(theFile, iSigs);
            if (Signature(curGroup) = 'GRUP') then begin
                for i:=0 to ElementCount(curGroup)-1 do begin
                    curRecord := ElementByIndex(curGroup, i);
                    if(IsMaster(curRecord)) then begin
                        Result := curRecord;
                        exit;
                    end;
                end;
            end;
        end;
    end;

    {
        Returns a formID with the value zero, but all the load order prefixes
        This is because xEdit has absolutely no way whatsoever to actually get the true load order of a file, even less so for ESLs.
        GetLoadOrder is a misnomer, it actually returns the index in the current list.
    }
    function GetZeroFormID(theFile: IwbFile): cardinal;
    var
        numMasters: integer;
        elemFormId, relativeFormID: cardinal;
        firstElem: IInterface;
    begin
        Result := 0;
        if(wbVersionNumber < XEDIT_VERSION_404) then begin
            numMasters := MasterCount(theFile);
            relativeFormID := (numMasters shl 24) and $FF000000;
            // this no longer works with xedit 4.0.4
            Result := FileFormIDtoLoadOrderFormID(theFile, relativeFormID);
        end else begin
            // try getting the first record in the thing
            firstElem := GetFirstNonOverrideElement(theFile);

            if(not assigned(firstElem)) then begin
                exit;
            end;
            elemFormId := GetLoadOrderFormID(firstElem);

            Result := getLoadOrderPrefix(theFile, elemFormId);
        end;
    end;


    {
        Returns the FormID of e with the LO prefix replaced with the corresponding master index in theFile.
        That is, if the record 0x00001234 is from the second master, this will return 0x01001234
    }
    function getRelativeFormId(theFile: IwbFile; e: IInterface): cardinal;
    var
        numMasters, i: integer;
        curMaster, mainRec, mainFile: IInterface;
    begin
        Result := 0;
        mainRec := MasterOrSelf(e);
        mainFile := GetFile(mainRec);
        numMasters := MasterCount(theFile);
        if(isSameFile(mainFile, theFile)) then begin
            // my own file
            Result := getLocalFormId(theFile, FormID(e)) or (numMasters shl 24);
            exit;
        end;

        for i:=0 to numMasters-1 do begin
            curMaster := MasterByIndex(theFile, i);
            if(isSameFile(mainFile, curMaster)) then begin
                // this file
                Result := getLocalFormId(theFile, FormID(e)) or (i shl 24);
                exit;
            end;
        end;
    end;

    {
        DEPRECATED: this is actually just the same as RecordByFormID.

        Returns an element by a formId, which is relative to the given file's master list.
        That is, if theFile has at least 2 masters and the given id is 0x01001234, it will
        try to find 0x00001234 in the second master.
        If applicable, will return the corresponding override from theFile.
    }
    function elementByRelativeFormId(theFile: IwbFile; id: cardinal): IInterface;
    var
        numMasters, prefix, baseId: integer;
        targetMaster, formMaster: IInterface;
    begin
        Result := RecordByFormID(theFile, id, true);
    end;

    {
        Strips the LO prefix from a FormID
    }
    function getLocalFormId(theFile: IwbFile; id: cardinal): cardinal;
    begin
        if(isFileLight(theFile)) then begin
            Result := $00000FFF and id;
        end else begin;
            Result := $00FFFFFF and id;
        end;
    end;

    {
        Strips the actual ID part from a FormID, leaving only the LO part
    }
    function getLoadOrderPrefix(theFile: IwbFile; id: cardinal): cardinal;
    begin
        if (isFileLight(theFile)) then begin
            Result := $FFFFF000 and id;
        end else begin
            Result := $FF000000 and id;
        end;
    end;

	function getElementLocalFormId(e: IInterface): cardinal;
	begin
		Result := getLocalFormId(GetFile(e), FormID(e));
	end;

    {
        An actually functional version of FileFormIDtoLoadOrderFormID.
    }
    function FileToLoadOrderFormID(theFile: IwbFile; id: cardinal): cardinal;
    var
        prefix: cardinal;
    begin
        prefix := GetZeroFormID(theFile);

        Result := prefix or id;
    end;

    {
        Like FileByLoadOrder, loads the file by actual load order ID, not by the number in the list (aka the 0xFF000000 part)
    }
    function FileByRealLoadOrder(loadOrder: cardinal): IInterface;
    var
        i: integer;
        id, mainLO, test: cardinal;
        curFile: IwbFile;
        curHeader: IInterface;
    begin
        // this sucks... but I have no better idea
        for i := 0 to FileCount-1 do
        begin
            curFile := FileByIndex(i);

            id := GetZeroFormID(curFile);
            mainLO := ($FF000000 and id) shr 24;
            if(mainLO = $FE) then begin
                continue;
            end;

            if(mainLO = loadOrder) then begin
                Result := curFile;
                exit;
            end;
        end;
    end;

    {
        Like FileByRealLoadOrder, but for light file load order (aka the 0x00FFF000 part)
    }
    function FileByLightLoadOrder(lightLoadOrder: cardinal): IInterface;
    var
        i: integer;
        id, mainLO, eslLO: cardinal;
        curFile: IwbFile;
    begin
        // this sucks... but I have no better idea
        for i := 0 to FileCount-1 do
        begin
            curFile := FileByIndex(i);

            id := GetZeroFormID(curFile);

            mainLO := ($FF000000 and id) shr 24;
            if(mainLO <> $FE) then begin
                continue;
            end;

            eslLO := ($FFF000 and id) shr 12;

            if(lightLoadOrder = eslLO) then begin
                Result := curFile;
                exit;
            end;
        end;
    end;

    function getFormByLoadOrderFormID(id: cardinal): IInterface;
    var
        lightLOIndex, loadOrderIndexInt, localFormId, fixedId, anotherFormId: cardinal;
        fileIndex: integer;
        theFile : IInterface;
        isLight : boolean;
        watFoo: string;
    begin
        Result := nil;

        loadOrderIndexInt := ($FF000000 and id) shr 24;


        if(loadOrderIndexInt = $FE) then begin
            // fix the formID for ESL
            lightLOIndex := ($FFF000 and id) shr 12;
            theFile := FileByLightLoadOrder(lightLOIndex);
            isLight := true;
        end else begin
            theFile := FileByRealLoadOrder(loadOrderIndexInt);
            isLight := false;
        end;

        if(not assigned(theFile)) then begin
            exit;
        end;

        Result := getFormByFileAndFormID(theFile, id);
    end;



    {
        Returns a record by it's prefix-less form ID and a file, like Game.GetFormFromFile does
    }
    function getFormByFileAndFormID(theFile: IInterface; id: cardinal): IInterface;
    var
        numMasters: integer;
        localFormId, fixedId, fileIndex: cardinal;
    begin
        // it looks like fileIndex is clamped to the count of masters, so numbers > than it will still produce the correct result
        fileIndex := MasterCount(theFile);

        Result := nil;

        fileIndex := (fileIndex shl 24) and $FF000000;

        fixedId := fileIndex or getLocalFormId(theFile, id);
        Result := RecordByFormID(theFile, fixedId, true);
    end;

    {
        Calculates a string's CRC32
        To output as string, use IntToHex(foo, 8)

        Function by zilav
    }
    function StringCRC32(s: string): Cardinal;
    var
        ms: TMemoryStream;
        bw: TBinaryWriter;
        br: TBinaryReader;
    begin
        ms := TMemoryStream.Create;
        bw := TBinaryWriter.Create(ms);
        bw.Write(s);
        bw.Free;
        ms.Position := 0;
        br := TBinaryReader.Create(ms);
        Result := wbCRC32Data(br.ReadBytes(ms.Size));
        br.Free;
        ms.Free;
    end;

    {
        Removes all characters which are not valid for an EditorID from the string.
    }
    function SanitizeEditorID(input: string): string;
    var
        i: integer;
        tmp, c: string;
    begin
        tmp := input;
        Result := '';

        for i:=1 to length(tmp) do begin
            c := tmp[i];

            if (
                (c >= 'a') and (c <= 'z') or
                (c >= 'A') and (c <= 'Z') or
                (c >= '0') and (c <= '9') or
                (c = '-') or (c = '_')
            ) then begin
                Result := Result + c;
            end;
        end;

    end;

    {
        Generates a string which is a valid EditorID: sanitizes the string, and shortens it using StringCRC32 if necessary.
        This should be deterministic in regard of input->output, but *MIGHT* not provide different outputs for different inputs...
    }
    function CreateValidEditorID(input: string): string;
    var
        inputSanitized, part: string;
    begin
        inputSanitized := SanitizeEditorID(input);
        Result := inputSanitized;

        if(length(inputSanitized) > MAX_EDID_LENGTH) then begin
            part := copy(inputSanitized, 1, MAX_EDID_LENGTH-9);
            Result := part + '_' + IntToHex(StringCRC32(inputSanitized), 8);
            exit;
        end;
    end;

    {
        Fixed version of IntToHex which will output a 8-char string representing a FormID without crashing due to overflow
    }
    function FormIdToHex(fid: cardinal): string;
    begin
        Result := IntToHex64(fid, 8); // it's still going to make 16 character long strings if the number is "negative"
        if(Length(Result) > 8) then begin
            Result := copy(Result, Length(Result)-8+1, 8);
        end;
    end;

    {
        Recursively outputs the given element into the given binary writer, for hashing purposes.
        The string isn't supposed to make much sense on it's own.
    }
    procedure WriteElementRecursive(e: IInterface; bw: TBinaryWriter; index: integer);
    var
        i: Integer;
        child, maybeLinksTo: IInterface
    begin
        for i := 0 to ElementCount(e)-1 do begin
            child := ElementByIndex(e, i);
            maybeLinksTo := LinksTo(child);
            // no clue how much is actually necessary here...
            bw.Write(IntToStr(index));
            bw.Write(';');
            bw.Write(DisplayName(child));
            bw.Write(';');
            if(assigned(maybeLinksTo)) then begin
                bw.Write(FormToAbsStr(child));
            end else begin
                bw.Write(GetEditValue(child));
            end;

            WriteElementRecursive(child, bw, index+1);
        end;

    end;

    {
        Calculates a CRC32 of the given element, by converting it into some sort of a string and hashing that.
        This isn't going to be compatible with any other implementation, but should return the same hash
        for equivalent elements.
    }
    function ElementCRC32(e: IInterface): string;
    var
        ms: TMemoryStream;
        bw: TBinaryWriter;
        br: TBinaryReader;
    begin
        ms := TMemoryStream.Create;
        bw := TBinaryWriter.Create(ms);

        WriteElementRecursive(e, bw, 0);

        bw.Free;
        ms.Position := 0;
        br := TBinaryReader.Create(ms);
        Result := wbCRC32Data(br.ReadBytes(ms.Size));
        br.Free;
        ms.Free;
    end;

    {
        Calculates a MD5 of the given element, by converting it into some sort of a string and hashing that.
        This isn't going to be compatible with any other implementation, but should return the same hash
        for equivalent elements.
    }
    function StringMD5(s: string): cardinal;
    var
        ms: TMemoryStream;
        bw: TBinaryWriter;
        br: TBinaryReader;
    begin
        ms := TMemoryStream.Create;
        bw := TBinaryWriter.Create(ms);
        bw.Write(s);
        bw.Free;
        ms.Position := 0;
        br := TBinaryReader.Create(ms);
        Result := wbMD5Data(br.ReadBytes(ms.Size));
        br.Free;
        ms.Free;
    end;

    {
        Tries to recursively create the given path. Returns the last subrecord on success. Returns nil on failure.
    }
    function ensurePath(elem: IInterface; path: string): IInterface;
    var
        i: integer;
        helper: TStringList;
        curPart, nextPart : IInterface;
    begin
        Result := ElementByPath(elem, path);
        if(assigned(Result)) then exit;

        curPart := elem;

        helper := TStringList.create;
        helper.Delimiter := '\';
        helper.StrictDelimiter := True; // Spaces excluded from being a delimiter
        helper.DelimitedText := path;

        for i := 0 to helper.count-1 do begin
            nextPart := ElementByName(curPart, helper[i]);
            if(not assigned(nextPart)) then begin
                nextPart := Add(elem, helper[i], true);
            end;

            if(not assigned(nextPart)) then begin
                // fail
                helper.free();
                exit;
            end;

            curPart := nextPart;
        end;

        Result := curPart;

        helper.free();
    end;

    function getFileAsJson(fullPath: string): TJsonObject;
    begin
        Result := TJsonObject.create();

        try
            Result.LoadFromFile(fullPath);
        except

            on E: Exception do begin
                AddMessage('Failed to parse '+fullPath+': '+E.Message);
                Result.free();
                Result := nil;
            end else begin
                AddMessage('Failed to parse '+fullPath+'.');
                Result.free();
                Result := nil;
            end;

            // code here seems to be unreachable
        end;
    end;

    {
        Fixed version of HighestOverrideOrSelf
    }
    function WinningOverrideOrSelf(e: IInterface): IInterface;
    begin
        Result := HighestOverrideOrSelf(e, 9000);
    end;

    {
        A potentially fixed version of seev, where path is created if it doesn't exist
    }
    procedure SetEditValueByPath(e: IInterface; path, value: string);
    var
        subrec: IInterface;
    begin
        subrec := ensurePath(e, path);
        if(assigned(subrec)) then begin
            SetEditValue(subrec, value);
        end;
    end;

    // Conversion functions
    {
        Returns "True" or "False"
    }
    function BoolToStr(b: boolean): string;
    begin
        if(b) then begin
            Result := 'True';
        end else begin
            Result := 'False';
        end;
    end;

    {
        Returns true for "true" (in any case), false otherwise
    }
    function StrToBool(s: string): boolean;
    begin
        Result := (LowerCase(s)  = 'true');
    end;

    function ternaryOp(condition: boolean; ifTrue: variant; ifFalse: variant): variant;
    begin
        if(condition) then begin
            Result := ifTrue;
            exit;
        end;
        Result := ifFalse;
    end;

    {
        because xEdit says that '%0001110000000000000000000000001' is not a valid integer value
    }
    function BinToInt(bin: string): cardinal;
    var
        curChar, tmp: string;
        i: integer;
        factor: cardinal;
    begin
        Result := 0;
        factor := 1;
        if(length(bin) > 64) then begin
            AddMessage('Binary string too long: '+bin);
            exit;
        end;

        tmp := bin;

        for i:=length(tmp) downto 1 do begin
            curChar := tmp[i];
            if(curChar = '1') then begin
                Result := (Result + factor);
            end else begin
                if(curChar <> '0') then begin
                    AddMessage(tmp+' is not a valid binary string');
                    Result := 0;
                    exit;
                end;
            end;

            factor := factor * 2;
        end;
    end;

    {
        Encodes the given form's ID into a string, so that it can be found again using that string.
		Bascially just gets the current LO formID as a hex string
    }
    function FormToStr(form: IInterface): string;
    var
        curFormID: cardinal;
    begin
        curFormID := GetLoadOrderFormID(MasterOrSelf(form));

        Result := FormIdToHex(curFormID);
    end;

    {
        Decodes a string generated by FormToStr into a FormID and finds the correspodning form
    }
    function StrToForm(str: string): IInterface;
    var
        theFormID: cardinal;
    begin
        Result := nil;
        if(str = '') then exit;
        // StrToInt64 must be used, otherwise large values will just cause an error
        theFormID := StrToInt64('$' + str);

        if(theFormID = 0) then exit;

        Result := getFormByLoadOrderFormID(theFormID);
    end;

	{
		Encodes a form into Filename:formID
	}
	function FormToAbsStr(form: IInterface): string;
	var
		theFile: IInterface;
		theFormId: cardinal;
		theFilename: string;
	begin
		theFile := GetFile(MasterOrSelf(form));
		theFilename := GetFileName(theFile);
		theFormId := getLocalFormId(theFile, FormID(form));

		Result := theFilename + ':'+FormIdToHex(theFormId);
	end;

	{
		Decodes a Filename:formID string into a form
	}
	function AbsStrToForm(str: string): IInterface;
	var
		separatorPos: integer;
		theFilename, formIdStr: string;
		theFormId: cardinal;
        regex: TPerlRegEx;
	begin
		Result := nil;
		separatorPos := Pos(':', str);
		if(separatorPos <= 0) then begin
			exit;
		end;


        regex := TPerlRegEx.Create();

        try
            regex.RegEx := '(.+):([0-9a-fA-F]+)';
            regex.Subject := str;

            if(regex.Match()) then begin
                // misnomer, is actually the highest valid index of regex.Groups
                if(regex.GroupCount >= 2) then begin
                    theFilename := regex.Groups[1];
                    formIdStr := regex.Groups[2];
                    Result := getFormByFilenameAndFormID(theFilename, StrToInt64('$'+formIdStr));
                end;
            end;
        finally
            RegEx.Free;
        end;
	end;

	function floatEqualsWithTolerance(val1, val2, tolerance: float): boolean;
	begin
		Result := abs(val1 - val2) < tolerance;
	end;

	function floatEquals(val1, val2: float): boolean;
	begin
		Result := floatEqualsWithTolerance(val1, val2, 0.0001);
	end;

    {
        Checks whenever a subrecord is any kind of array
    }
    function isSubrecordArray(e: IInterface): boolean;
    var
        t1: TwbElementType;
        t2: TwbDefType;
    begin
        Result := false;
        t1 := ElementType(e);

        if (t1 = etSubRecordArray) or (t1 = etArray) then begin
            Result := true;
            exit;
        end;

        t2 := DefType(e);

        if (t2 = dtSubRecordArray) or (t2 = dtByteArray) or (t2 = dtArray) then begin
            Result := true;
            exit;
        end;
    end;

    {
        Checks whenever a subrecord is something non-iterable, basically
    }
    function isSubrecordScalar(e: IInterface): boolean;
    var
        t1: TwbElementType;
        t2: TwbDefType;
    begin
        Result := false;
        t1 := ElementType(e);

        if (t1 = etSubRecordArray) or (t1 = etArray) or (t1 = etMainRecord) or (t1 = etGroupRecord) or (t1 = etSubRecordStruct) or (t1 = etSubRecordArray) or (t1 = etSubRecordUnion)
            or (t1 = etArray) or (t1 = etStruct) or (t1 = etUnion)
        then begin
            Result := false;
            exit;
        end;

        if (t1 = etFlag) or (t1 = etValue) then begin
            Result := true;
            exit;
        end;

        t2 := DefType(e);

        if (t2 = dtSubRecordArray) or (t2 = dtByteArray) or (t2 = dtArray) or (t2 = dtSubRecordStruct) or (t2 = dtSubRecordUnion)
            or (t2 = dtStruct) or (t2 = dtUnion)
        then begin
            Result := false;
            exit;
        end;

        if (t2 = dtString) or (t2 = dtLString) or (t2 = dtLenString) or (t2 = dtInteger) or (t2 = dtFloat) or (t2 = dtEmpty) then begin
            Result := true;
            exit;
        end;
    end;

    {
        Checks whenever the element is modified, but not saved (bold in xEdit)
    }
    function isElementUnsaved(e: IInterface): boolean;
    begin
        Result := GetElementState(e, 2);
    end;

    {

    // other flags which could be checked:
    function IntToEsState(anInt: Integer): TwbElementState;
    begin
      case anInt of
        0: Result := esModified;
        1: Result := esInternalModified;
        2: Result := esUnsaved;
        3: Result := esSortKeyValid;
        4: Result := esExtendedSortKeyValid;
        5: Result := esHidden;
        6: Result := esParentHidden;
        7: Result := esParentHiddenChecked;
        8: Result := esNotReachable;
        9: Result := esReachable;
        10: Result := esTagged;
        11: Result := esResolving;
        12: Result := esNotSuitableToAddTo;
      else
        Result := esDummy;
      end;
    end;
    }

    // Keyword-manipulation functions
    {
        Adds a keyword to a specific signature. KWYD is the most usual one
    }
    procedure addKeywordByPath(toElem: IInterface; kw: IInterface; targetSig: string);
    var
        container: IInterface;
        newElem: IInterface;
        num: integer;
        formId: LongWord;
    begin
        container := ElementByPath(toElem, targetSig);
        num := ElementCount(container);

        if((not assigned(container)) or (num <= 0)) then begin
            container := Add(toElem, targetSig, True);
        end;

        newElem := ElementAssign(container, HighInteger, nil, False);
        formId := GetLoadOrderFormID(kw);
        SetEditValue(newElem, FormIdToHex(formId));
    end;

    function hasKeywordByPath(e: IInterface; kw: variant; signature: String): boolean;
    var
        kwda: IInterface;
        curKW: IInterface;
        i, variantType: Integer;
        kwEdid: string;
    begin
        Result := false;
        kwda := ElementByPath(e, signature);

        variantType := varType(kw);
        if (variantType = varUString) or (variantType = varString) then begin
            kwEdid := kw;
        end else begin
            kwEdid := EditorID(kw);
        end;

        for i := 0 to ElementCount(kwda)-1 do begin
            curKW := LinksTo(ElementByIndex(kwda, i));

            if EditorID(curKW) = kwEdid then begin
                Result := true;
                exit;
            end
        end;
    end;

    procedure ensureKeywordByPath(toElem: IInterface; kw: IInterface; targetSig: string);
    begin
        if(not hasKeywordByPath(toElem, kw, targetSig)) then begin
            addKeywordByPath(toElem, kw, targetSig);
        end;
    end;

    procedure removeKeywordByPath(e: IInterface; kw: variant; signature: String);
    var
        kwda: IInterface;
        curKW, kwdaEntry: IInterface;
        i, variantType: Integer;
        kwEdid: string;
    begin
        kwda := ElementByPath(e, signature);

        variantType := varType(kw);
        if (variantType = varUString) or (variantType = varString) then begin
            kwEdid := kw;
        end else begin
            kwEdid := EditorID(kw);
        end;

        for i := 0 to ElementCount(kwda)-1 do begin
            kwdaEntry := ElementByIndex(kwda, i);
            curKW := LinksTo(kwdaEntry);
            if (EditorID(curKW) = kwEdid) then begin
                // this seems to be more reliable than by index
                RemoveElement(kwda, kwdaEntry);
                exit;
            end
        end;
    end;

    function getAvByPath(e: IInterface; av: variant; signature: string): float;
    var
        kwda, curKW, curProp: IInterface;
        i, variantType: Integer;
        kwEdid: string;
    begin
        Result := 0.0;
        kwda := ElementByPath(e, signature);

        variantType := varType(av);
        if (variantType = varUString) or (variantType = varString) then begin
            kwEdid := av;
        end else begin
            kwEdid := EditorID(av);
        end;

        for i := 0 to ElementCount(kwda)-1 do begin
            curProp := ElementByIndex(kwda, i);

            curKw := pathLinksTo(curProp, 'Actor Value');

            if EditorID(curKW) = kwEdid then begin
                Result := StrToFloat(GetElementEditValues(curKw, 'Value'));
                exit;
            end
        end;

    end;

    // linkage
    {
        Returns whatever `fromRef` might be linked to using `usingKw`
    }
    function getLinkedRef(fromRef, usingKw: IInterface): IInterface;
    var
        i: integer;
        outLinks, lnk, kw: IInterface;
    begin
        Result := nil;
        outLinks := ElementByPath(fromRef, 'Linked References');
        if(not assigned(outLinks)) then exit;

        for i:=0 to ElementCount(outLinks)-1 do begin
            lnk := ElementByIndex(outLinks, i);

            kw := pathLinksTo(lnk, 'Keyword/Ref');

            if(not assigned(usingKw)) then begin
                if(not assigned(kw)) then begin
                    Result := pathLinksTo(lnk, 'Ref');
                    exit;
                end;
            end else begin
                if (isSameForm(kw, usingKw)) then begin
                    Result := pathLinksTo(lnk, 'Ref');
                    exit;
                end;
            end;
        end;
    end;

    {
        Returns a TList of ObjectReferences which are linked to `toRef` using `usingKw`
    }
    function getLinkedRefChildren(toRef, usingKw: IInterface): TList;
    var
        i: integer;
        curRef, linkBack: IInterface;
        dupeCheckList: TStringList;
        lookupKey: string;
    begin
        Result := TList.create;
        dupeCheckList := TStringList.create;

        for i:= 0 to ReferencedByCount(toRef)-1 do begin
            curRef := WinningOverrideOrSelf(ReferencedByIndex(toRef, i));
            // linked?
            linkBack := getLinkedRef(curRef, usingKw);

            if (isSameForm(linkBack, toRef)) then begin
                // must be deduplicated
                lookupKey := FormToStr(curRef);
                if(dupeCheckList.indexOf(lookupKey) < 0) then begin
                    dupeCheckList.add(lookupKey);
                    Result.add(curRef);
                end;
            end;
        end;

        dupeCheckList.free();
    end;

    // Formlist-Manipulation functions

    {
        Looks in the given formlist for an enthry with the given edid, and if found, returns it
    }
    function getFormlistEntryByEdid(formList: IInterface; edid: string): IInterface;
    var
        numElems, i : integer;
        curElem: IInterface;
        formIdList: IInterface;
    begin
        Result := nil;
        formIdList := ElementByName(formList, 'FormIDs');
        if(assigned(formIdList)) then begin
            numElems := ElementCount(formIdList);

            if(numElems > 0) then begin

                for i := 0 to numElems-1 do begin
                    curElem := LinksTo(ElementByIndex(formIdList, i));

                    if(geevt(curElem, 'EDID') = edid) then begin
                        Result := curElem;
                        exit;
                    end;
                end;

            end;
        end;
    end;

    {
        Checks whenever the given formlist has the given entry
    }
    function hasFormlistEntry(formList: IInterface; entry: IInterface): boolean;
    var
        numElems, i : integer;
        curElem: IInterface;
        formIdList: IInterface;
    begin
        Result := false;
        formIdList := ElementByName(formList, 'FormIDs');
        if(assigned(formIdList)) then begin
            numElems := ElementCount(formIdList);

            if(numElems > 0) then begin

                for i := 0 to numElems-1 do begin
                    curElem := LinksTo(ElementByIndex(formIdList, i));

                    if(isSameForm(curElem, entry)) then begin
                        Result := true;
                        exit;
                    end;
                end;

            end;
        end;
    end;

    {
        Adds a form to a formlist, if it doesn't exist already
    }
    procedure addToFormlist(formList: IInterface; newForm: IInterface);
    var
        numElems, i : integer;
        curElem: IInterface;
        formIdList: IInterface;
    begin

        if(not assigned(newForm)) or (GetLoadOrderFormID(newForm) = 0) then begin
            exit;
        end;


        formIdList := ElementByName(formList, 'FormIDs');
        if(not assigned(formIdList)) then begin
            formIdList := Add(formList, 'FormIDs', True);
            // This automatically gives you one free entry pointing to NULL
            curElem := ElementByIndex(formIdList, i);
            SetEditValue(curElem, FormIdToHex(GetLoadOrderFormID(newForm)));
            exit;
        end;


        numElems := ElementCount(formIdList);

        if(numElems > 0) then begin
            for i := 0 to numElems-1 do begin
                curElem := LinksTo(ElementByIndex(formIdList, i));
                if(isSameForm(curElem, newForm)) then begin
                    exit;
                end;
            end;
        end;


        curElem := ElementAssign(formIdList, HighInteger, nil, False);
        SetEditValue(curElem, FormIdToHex(GetLoadOrderFormID(newForm)));

    end;

    {
        Removes everything from the formlist
    }
    procedure clearFormList(formList: IInterface);
    var
        formIdList: IInterface;
    begin
        // levelFormlist
        formIdList := ElementByName(formList, 'FormIDs');
        if(assigned(formIdList)) then begin
            RemoveElement(formList, formIdList);
        end;
    end;

    {
        Gets the length of a formlist
    }
    function getFormListLength(formList: IInterface): integer;
    var
        formIdList: IInterface;
    begin
        formIdList := ElementByName(formList, 'FormIDs');
        Result := 0;
        if(not assigned(formIdList)) then begin
            exit;
        end;
        Result := ElementCount(formIdList);
    end;

    {
        Gets a specific element from a formlist
    }
    function getFormListEntry(formList: IInterface; index: integer): IInterface;
    var
        formIdList: IInterface;
    begin
        Result := nil;
        formIdList := ElementByName(formList, 'FormIDs');
        if(not assigned(formIdList)) then begin
            exit;
        end;
        Result := LinksTo(ElementByIndex(formIdList, index));
    end;

    {
        Creates a new entry at the end of an array-like element at path.
        Takes care of the free first item automatically.
    }
    function addNewEntry(elem: IInterface; path: string): IInterface;
    var
        elemAtPath: IInterface;
    begin
        elemAtPath := ElementByPath(elem, path);

        if(assigned(elemAtPath)) then begin
            Result := ElementAssign(elemAtPath, HighInteger, nil, False);
            exit;
        end;

        elemAtPath := EnsurePath(elem, path);
        // see if we got the free elem
        if(ElementCount(elemAtPath) = 1) then begin
            Result := ElementByIndex(elemAtPath, 0);
            exit;
        end;

        Result := ElementAssign(elemAtPath, HighInteger, nil, False);
    end;

    {
        Removes the elem located at path from elem.
    }
    procedure removeByPath(elem: IInterface; path: string);
    var
        i, len: integer;
        pathTmp, curChar, pathParent, pathChild: string;
        parent, child: IInterface;
    begin
        // find the last \
        pathTmp := path;
        pathParent := pathTmp;
        len := length(path);

        for i:=len downto 1 do begin
            curChar := copy(pathTmp, i, 1);

            if (curChar = '\') or (curChar = '/') then begin
                pathParent := copy(pathTmp, 1, i-1);
                pathChild := copy(pathTmp, i+1, len-i);
                break;
            end;
        end;

        if(pathChild = '') then begin
            // easy
            child := ElementByPath(pathParent);
            if(assigned(child)) then begin
                RemoveElement(elem, child);
            end;
        end;

        parent := ElementByPath(elem, pathParent);
        if(not assigned(parent)) then begin
            exit;
        end;

        child := ElementByPath(parent, pathChild);
        if(not assigned(child)) then begin
            exit;
        end;

        RemoveElement(parent, child);
    end;

    // helper functions
    function strContainsCI(haystack: String; needle: String): boolean;
    begin
        Result := pos(LowerCase(needle), LowerCase(haystack)) <> 0;
    end;


    {
        Checks if string haystack starts with string needle
    }
    function strStartsWith(haystack: String; needle: String): boolean;
    var
        len: Integer;
        cmp: String;
    begin
        if needle = haystack then begin
            Result := true;
            exit;
        end;

        len := length(needle);

        if len > length(haystack) then begin
            Result := false;
            exit;
        end;

        cmp := copy(haystack, 0, len);

        Result := (cmp = needle);
    end;

    function strStartsWithCI(haystack: String; needle: String): boolean;
    begin
        Result := strStartsWith(LowerCase(haystack), LowerCase(needle));
    end;

    {
        Checks if string haystack ends with string needle
    }
    function strEndsWith(haystack: String; needle: String): boolean;
    var
        len, lenHaystack: Integer;
        cmp: String;
    begin
        if needle = haystack then begin
            Result := true;
            exit;
        end;

        len := length(needle);
        lenHaystack := length(haystack);

        if len > lenHaystack then begin
            Result := false;
            exit;
        end;

        cmp := copy(haystack, lenHaystack-len+1, lenHaystack);

        Result := (cmp = needle);
    end;

    function strEndsWithCI(haystack: String; needle: String): boolean;
    begin
        Result := strEndsWith(LowerCase(haystack), LowerCase(needle));
    end;

    function getStringAfter(str, separator: string): string;
    var
        p: integer;
    begin

        p := Pos(separator, str);
        if(p <= 0) then begin
            Result := str;
            exit;
        end;

        p := p + length(separator);

        Result := copy(str, p, length(str)-p+1);
    end;

	function StringRepeat(str: string; len: integer): string;
	var
		i: integer;
	begin
		Result := '';
		for i:=0 to len-1 do begin
			Result := Result + str;
		end;
	end;

    function StringReverse(s: string): string;
    var
        i, len: integer;
    begin
        Result := '';
        len := Length(s);
        for i := len downto 1 do begin
            Result := Result + Copy(s, i, 1);
        end;
    end;

    function strUpperCaseFirst(str: string): string;
    var
        firstChar, rest: string;
        len: integer;
    begin
        len := length(str);

        firstChar := copy(str, 0, 1);
        rest := copy(str, 2, len);

        Result := UpperCase(firstChar) + LowerCase(rest);
    end;

    {
        Checks if f1 is being referenced by f2.
        Possible usecase: pass a formlist as f2 for faster lookup
    }
    function isReferencedBy(f1, f2: IInterface): boolean;
    var
        numRefs, i: integer;
        curRec: IInterface;
    begin
        Result := false;

        numRefs := ReferencedByCount(f1)-1;
        for i := 0 to numRefs do begin
            curRec := ReferencedByIndex(f1, i);
            if(isSameForm(curRec, f2)) then begin
                Result := true;
                exit;
            end;
        end;
    end;

    {
        Checks if the two objects are the same, because IInterfaces aren't comparable
    }
    function isSameForm(e1: IInterface; e2: IInterface): boolean;
    begin
        Result := Equals(MasterOrSelf(e1), MasterOrSelf(e2));
    end;

    function FormsEqual(e1: IInterface; e2: IInterface): boolean;
    begin
        Result := isSameForm(e1, e2);
    end;

    {
        Setter to the getter LinksTo. formToAdd can be nil, to set the property to none
    }
    procedure setLinksTo(e: IInterface; formToAdd: IInterface);
    begin
        if(assigned(formToAdd)) then begin
            SetEditValue(e, FormIdToHex(GetLoadOrderFormID(formToAdd)));
        end else begin
            SetEditValue(e, FormIdToHex(0));
        end;
    end;

    {
        A combination of SetElementEditValues and SetLinksTo: sets the value at the given path to the given form
    }
    procedure setPathLinksTo(e: IInterface; path: string; form: IInterface);
    begin
        if(assigned(form)) then begin
            SetElementEditValues(e, path, FormIdToHex(GetLoadOrderFormID(form)));
        end else begin
            SetElementEditValues(e, path, FormIdToHex(0));
        end;
    end;

    {
        A combination of ElementByPath and LinksTo: returns what the element links to at the given path
    }
    function pathLinksTo(e: IInterface; path: string): IInterface;
    begin
        Result := LinksTo(ElementByPath(e, path));
    end;

    {
        Checks whenever the element has the given flag set
    }
    function hasFlag(e: IInterface; flagName: string): boolean;
    var
        i: integer;
        curName, curValue: string;
    begin
        Result := false;

        for i:=0 to ElementCount(e)-1 do begin
            curName := DisplayName(ElementByIndex(e, i));
            curValue := GetEditValue(ElementByIndex(e, i));
            if (curName = flagName) and (curValue = '1') then begin
                Result := true;
                exit;
            end;
        end
    end;

    {
        similar to GetElementByPath, but will create everything along the path if it doesn't exist

        Note: here, the path can only contain signatures.
            Good: 'VMAD\Scripts'
            Bad:  'VMAD - Virtual Machine Adapter\Scripts'
    }
    function CreateElementByPath(e: IInterface; objectPath: string): IInterface;
    var
        i, index: integer;
        path: TStringList;
        curSubpath: IInterface;
    begin
        // replace forward slashes with backslashes
        objectPath := StringReplace(objectPath, '/', '\', [rfReplaceAll]);

        // prepare path stringlist delimited by backslashes
        path := TStringList.Create;
        path.Delimiter := '\';
        path.StrictDelimiter := true;
        path.DelimitedText := objectPath;

        curSubpath := e;

        // traverse path
        for i := 0 to Pred(path.count) do begin
            curSubpath := ElementByPath(e, path[i]);
            if(not assigned(curSubpath)) then begin
                curSubpath := Add(e, path[i], true);
            end;
            e := curSubpath;
        end;

        // set result
        Result := e;
    end;

    // script functions
    {
        Get a script by name
    }
    function getScript(e: IInterface; scriptName: String): IInterface;
    var
        curScript, scripts: IInterface;
        i: integer;
    begin
        Result := nil;
        scripts := ElementByPath(e, 'VMAD - Virtual Machine Adapter\Scripts');

        scriptName := LowerCase(scriptName);

        for i := 0 to ElementCount(scripts)-1 do begin
            curScript := ElementByIndex(scripts, i);

            if(LowerCase(geevt(curScript, 'scriptName')) = scriptName) then begin
                Result := curScript;
                exit;
            end;
        end;
    end;

    {
        Returns the fragment script of the given form, if it has any
    }
    function getFragmentScript(e: IInterface): IInterface;
    var
        curScript, scripts: IInterface;
        i: integer;
    begin
        Result := ElementByPath(e, 'VMAD - Virtual Machine Adapter\Script Fragments\Script');
    end;

    {
        Get the first script in the element, no matter what
    }
    function getFirstScript(e: IInterface): IInterface;
    var
        curScript, scripts: IInterface;
        i: integer;
    begin
        Result := nil;
        scripts := ElementByPath(e, 'VMAD - Virtual Machine Adapter\Scripts');

        for i := 0 to ElementCount(scripts)-1 do begin
            Result := ElementByIndex(scripts, i);
            exit;
        end;
    end;

    {
        Gets the name for the first script, in case the object type or such depends on that
    }
    function getFirstScriptName(e: IInterface): string;
    var
        curScript, scripts: IInterface;
        i: integer;
    begin
        Result := '';
        curScript := getFirstScript(e);
        if(not assigned(curScript)) then exit;

        Result := GetElementEditValues(curScript, 'scriptName');
    end;

    {
        Like getScript, but if it doesn't exist, it will be added
    }
    function addScript(e: IInterface; scriptName: String): IInterface;
    var
        curScript, scripts: IInterface;
        i: integer;
    begin
        Result := nil;
        scripts := createElementByPath(e, 'VMAD\Scripts');


        for i := 0 to ElementCount(scripts)-1 do begin
            curScript := ElementByIndex(scripts, i);

            if(geevt(curScript, 'scriptName') = scriptName) then begin
                Result := curScript;
                exit;
            end;
        end;

        // otherwise append
        Result := ElementAssign(scripts, HighInteger, nil, False);//Add(propRoot, 'Property', true);
        SetElementEditValues(Result, 'scriptName', scriptName);
    end;

    {
        Gets a script property by name. Returns the raw IInterface representing it
    }
    function getRawScriptProp(script: IInterface; propName: String): IInterface;
    var
        propRoot, prop: IInterface;
        i: integer;
    begin
        propRoot := ElementByPath(script, 'Properties');
        Result := nil;

        if(not assigned(propRoot)) then begin
            exit;
        end;

        propName := LowerCase(propName);

        for i := 0 to ElementCount(propRoot)-1 do begin
            prop := ElementByIndex(propRoot, i);

            if(LowerCase(geevt(prop, 'propertyName')) = propName) then begin
                Result := prop;
                exit;
            end;
        end;
    end;

    {
        Gets a struct member by name. Returns the raw IInterface representing it
    }
    function getRawStructMember(struct: IInterface; memberName: String): IInterface;
    var
        member: IInterface;
        i: integer;
    begin
        Result := nil;

        memberName := LowerCase(memberName);

        for i := 0 to ElementCount(struct)-1 do begin
            member := ElementByIndex(struct, i);

            if(LowerCase(geevt(member, 'memberName')) = memberName) then begin
                Result := member;
                exit;
            end;
        end;
    end;

    {
        Gets a script property by name. Returns different things, depending on the type:
        - Int32: returns integer
        - Float: returns float
        - String: retuns string
        - Bool: returns boolean
        - Object: resolves what the property links to, returns IInterface
        - Struct: returns the raw property value
        - Any array: returns the raw property value
        - property doesn't exist: returns nil
    }
    function getScriptProp(script: IInterface; propName: String): variant;
    begin
        Result := getScriptPropDefault(script, propName, nil);
    end;

    {
        Extracts the value of a script property or struct member, returns a variant representing it
    }
    function getValueAsVariant(prop: IInterface; defaultValue: variant): variant;
    var
        typeStr, valueString: string;
        propVal: IInterface;
    begin
        typeStr := geevt(prop, 'Type');
        if(strStartsWith(typeStr, 'Array of') or (typeStr = 'Struct')) then begin

            Result := ElementByPath(prop, 'Value\'+typeStr);
            exit;
        end;

        // easy types
        if(typeStr = 'String') then begin
            Result := geevt(prop, typeStr);
            exit;
        end;

        if(typeStr = 'Int32') then begin
            Result := StrToInt(geevt(prop, typeStr));
            exit;
        end;

        if(typeStr = 'Float') then begin
            Result := StrToFloat(geevt(prop, typeStr));
            exit;
        end;

        if(typeStr = 'Bool') then begin
            Result := StrToBool(geevt(prop, typeStr));
            exit;
        end;

        // Object
        if(typeStr = 'Object') then begin
            propVal := ElementByPath(prop, 'Value\Object Union\Object v2\FormID');
            Result := LinksTo(propVal);
            exit;
        end;

        Result := defaultValue;
    end;

    {
        like getScriptProp, but if the property doesn't exist, the given default value will be returned
    }
    function getScriptPropDefault(script: IInterface; propName: String; defaultValue: variant): variant;
    var
        prop, propVal: IInterface;
        typeStr: string;
    begin
        prop := getRawScriptProp(script, propName);
        if(not assigned(prop)) then begin
            Result := defaultValue;
            exit;
        end;

        Result := getValueAsVariant(prop, defaultValue);
    end;

    function getScriptPropType(script: IInterface; propName: String): string;
    var
        prop, propVal: IInterface;
        typeStr: string;
    begin
        prop := getRawScriptProp(script, propName);
        if(not assigned(prop)) then begin
            Result := '';
            exit;
        end;

        Result := geevt(prop, 'Type');
    end;

    {
        Creates a raw script property and returns it
    }
    function createRawScriptProp(script: IInterface; propName: String): IInterface;
    var
        propRoot, prop: IInterface;
        i: integer;
    begin
        propRoot := ElementByPath(script, 'Properties');

        if(not assigned(propRoot)) then begin
            // try creating
            propRoot := Add(script, 'Properties', true);
            if(not assigned(propRoot)) then begin
                AddMessage('ERROR: SCRIPT HAS NO PROPERTIES. THIS IS BAD');
                exit;
            end;
        end;

        Result := nil;

        for i := 0 to ElementCount(propRoot)-1 do begin
            prop := ElementByIndex(propRoot, i);

            if(geevt(prop, 'propertyName') = propName) then begin

                Result := prop;
                exit;
            end;
        end;

        // if still alive, somehow append
        Result := ElementAssign(propRoot, HighInteger, nil, False);//Add(propRoot, 'Property', true);

        SetElementEditValues(Result, 'propertyName', propName);
    end;

    {
        Deletes a script property by name
    }
    procedure deleteScriptProp(script: IInterface; propName: String);
    var
        propRoot, prop: IInterface;
        i: integer;
    begin
        propRoot := ElementByPath(script, 'Properties');

        if(not assigned(propRoot)) then begin
            exit;
        end;

        for i := 0 to ElementCount(propRoot)-1 do begin
            prop := ElementByIndex(propRoot, i);

            if(geevt(prop, 'propertyName') = propName) then begin
                RemoveElement(propRoot, prop);
                exit;
            end;
        end;

    end;

    procedure deleteScriptProps(script: IInterface);
    var
        propRoot: IInterface;
    begin
        propRoot := ElementByPath(script, 'Properties');
        while (ElementCount(propRoot) > 0) do begin
            RemoveElement(propRoot, 0);
        end;
        // RemoveElement(script, propRoot);
    end;

    {
        Creates a raw struct member and returns it
    }
    function createRawStructMember(struct: IInterface; memberName: String): IInterface;
    var
        prop: IInterface;
        i: integer;
    begin
        Result := nil;

        for i := 0 to ElementCount(struct)-1 do begin
            prop := ElementByIndex(struct, i);

            if(geevt(prop, 'memberName') = memberName) then begin

                Result := prop;
                exit;
            end;
        end;
        // if still alive, somehow append
        Result := ElementAssign(struct, HighInteger, nil, False);

        SetElementEditValues(Result, 'memberName', memberName);
    end;

    {
        Delete a struct member by name
    }
    procedure deleteStructMember(struct: IInterface; memberName: String);
    var
        prop: IInterface;
        i: integer;
    begin

        for i := 0 to ElementCount(struct)-1 do begin
            prop := ElementByIndex(struct, i);

            if(geevt(prop, 'memberName') = memberName) then begin
                RemoveElement(struct, prop);
                exit;
            end;
        end;
    end;

    {
        Get or create a script property with a specified type. The result will have the given type, no matter what it had before
    }
    function getOrCreateScriptProp(script: IInterface; propName: String; propType: String): IInterface;
    begin
        Result := createRawScriptProp(script, propName);

        SetElementEditValues(Result, 'Type', propType);
    end;

    function getOrCreateScriptPropStruct(script: IInterface; propName: String): IInterface;
    begin
        Result := createRawScriptProp(script, propName);

        SetElementEditValues(Result, 'Type', 'Struct');

        Result := ElementByPath(Result, 'Value\Struct');
    end;

    function getOrCreateScriptPropArrayOfObject(script: IInterface; propName: String): IInterface;
    begin
        Result := createRawScriptProp(script, propName);

        SetElementEditValues(Result, 'Type', 'Array of Object');

        Result := ElementByPath(Result, 'Value\Array of Object');
    end;

    function getOrCreateScriptPropArrayOfStruct(script: IInterface; propName: String): IInterface;
    begin
        Result := createRawScriptProp(script, propName);

        SetElementEditValues(Result, 'Type', 'Array of Struct');

        Result := ElementByPath(Result, 'Value\Array of Struct');
    end;

    {
        Mostly a copy of ElementTypeString from mteFunctions, somewhat optimized
    }
    function getElementTypeString(e: IInterface): string;
    begin
        case ElementType(e) of
            etFile:                 Result := 'etFile';
            etMainRecord:           Result := 'etMainRecord';
            etGroupRecord:          Result := 'etGroupRecord';
            etSubRecord:            Result := 'etSubRecord';
            etSubRecordStruct:      Result := 'etSubRecordStruct';
            etSubRecordArray:       Result := 'etSubRecordArray';
            etSubRecordUnion:       Result := 'etSubRecordUnion';
            etArray:                Result := 'etArray';
            etStruct:               Result := 'etStruct';
            etValue:                Result := 'etValue';
            etFlag:                 Result := 'etFlag';
            etStringListTerminator: Result := 'etStringListTerminator';
            etUnion:                Result := 'etUnion';
            else                    Result := '';
        end;
    end;

    function getVarTypeString(x: variant): string;
    var
        basicType  : Integer;
    begin
        basicType := VarType(x);// and VarTypeMask;

        // Set a string to match the type
        case basicType of
            varEmpty     : Result := 'varEmpty';
            varNull      : Result := 'varNull';
            varSmallInt  : Result := 'varSmallInt';
            varInteger   : Result := 'varInteger';
            varSingle    : Result := 'varSingle';
            varDouble    : Result := 'varDouble';
            varCurrency  : Result := 'varCurrency';
            varDate      : Result := 'varDate';
            varOleStr    : Result := 'varOleStr';
            varDispatch  : Result := 'varDispatch';
            varError     : Result := 'varError';
            varBoolean   : Result := 'varBoolean';
            varVariant   : Result := 'varVariant';
            varUnknown   : Result := 'varUnknown';
            varByte      : Result := 'varByte';
            varWord      : Result := 'varWord';
            varLongWord  : Result := 'varLongWord';
            //vart64       : Result := 'vart64'; // doesn't seem to exist
            varStrArg    : Result := 'varStrArg';
            varString    : Result := 'varString';
            varUString     : Result := 'varUString ';
            varAny       : Result := 'varAny';
            varTypeMask  : Result := 'varTypeMask';
            else       Result := IntToStr(basicType);
        end;
    end;

    {
        Returns whenever the given signature is the signature of any objectreference
    }
    function isReferenceSignature(sig: string): boolean;
    begin
        // todo are these all?
        Result := ( (sig = 'REFR') or (sig = 'ACHR') or (sig = 'PGRE') or (sig = 'PHZD') );
    end;

    {
        Set the value of a raw script property or struct member
    }
    procedure setPropertyValue(propElem: IInterface; value: variant);
    var
        iinterfaceTypeString: string;
        variantType: integer;
    begin
        variantType := varType(value);

        if (variantType = 277) or (variantType = varNull) then begin// No idea if this constant exists
            // consider nil to be an empty form
            SetElementEditValues(propElem, 'Type', 'Object');

            SetLinksTo(ElementByPath(propElem, 'Value\Object Union\Object v2\FormID'), nil);
            exit;
        end;

        if(variantType = varUnknown) then begin
            // etMainRecord -> object, do a linksTo
            iinterfaceTypeString := getElementTypeString(value);
            if(iinterfaceTypeString = 'etMainRecord') then begin
                SetElementEditValues(propElem, 'Type', 'Object');

                SetLinksTo(ElementByPath(propElem, 'Value\Object Union\Object v2\FormID'), value);
            end; // else maybe struct?
        end else if (variantType = varInteger) or (variantType = 20) then begin
            // 20 is cardinal, no idea if there's a constant for that
            SetElementEditValues(propElem, 'Type', 'Int32');
            SetElementEditValues(propElem, 'Int32', IntToStr(value));
        end else if(variantType = varDouble) then begin
            SetElementEditValues(propElem, 'Type', 'Float');
            SetElementEditValues(propElem, 'Float', FloatToStr(value));
        end else if(variantType = varUString ) or (variantType =varString) then begin
            SetElementEditValues(propElem, 'Type', 'String');
            SetElementEditValues(propElem, 'String', value);
        end else if(variantType = varBoolean) then begin
            SetElementEditValues(propElem, 'Type', 'Bool');
            SetElementEditValues(propElem, 'Bool', BoolToStr(value));
        end else begin
            AddMessage('Unknown type in setPropertyValue! '+IntToStr(variantType));
        end;
    end;

    {
        Checks whenever the given value has a type which can be set as a property/struct member
    }
    function isVariantValidForProperty(value: variant): boolean;
    var
        variantType: integer;
        iinterfaceTypeString: string;
        propElem: IInterface;
    begin
        variantType := varType(value);
        Result := true;

        if (variantType = 277) then begin// No idea if this constant exists
            Result := false;
            exit;
        end;

        if(variantType = varUnknown) then begin
            if(getElementTypeString(value) = 'etMainRecord') then begin
                Result := true;
                exit;
            end;
            Result := false;
            exit;
        end;


    end;

    {
        Set a script property. Cannot set the value to structs or arrays.
    }
    procedure setScriptProp(script: IInterface; propName: string; value: variant);
    var
        propElem: IInterface;
    begin
        if(not isVariantValidForProperty(value)) then begin
            exit;
        end;

        propElem := createRawScriptProp(script, propName);
        setPropertyValue(propElem, value);
    end;

    procedure setScriptPropDefault(script: IInterface; propName: string; value, default: variant);
    var
        prevValue: variant;
    begin
        if(value = default) then begin
            // check if we should clean out the existing value
            prevValue := getScriptProp(script, propName);

            if(prevValue <> default) then begin
                deleteScriptProp(script, propName);
            end
            exit;
        end

        setScriptProp(script, propName, value);
    end;

    {
        Set a struct member. Cannot set the value to arrays.
    }
    procedure setStructMember(struct: IInterface; memberName: string; value: variant);
    var
        propElem: IInterface;
    begin
        if(not isVariantValidForProperty(value)) then begin
            exit;
        end;

        propElem := createRawStructMember(struct, memberName);
        setPropertyValue(propElem, value);
    end;

    {
        Like setStructMember, but won't do anything if value is equal to default
    }
    procedure setStructMemberDefault(struct: IInterface; memberName: string; value, default: variant);
    begin
        if(value = default) then begin
            exit;
        end

        setStructMember(struct, memberName, value);
    end;

    {
        Remove any value from the given raw property or struct member
    }
    procedure clearProperty(prop: IInterface);
    var
        value: IInterface;
        typeStr: string;
    begin
        typeStr := geevt(prop, 'Type');

        if(typeStr = '') then begin
            // assume it's an array
            clearArrayProperty(prop);
        end;

        // "If it's stupid, but works, ..."
        SetElementEditValues(prop, 'Type', 'Bool');
        SetElementEditValues(prop, 'Bool', 'False');
        SetElementEditValues(prop, 'Type', typeStr);
    end;

    {
        For when the prop is Value\Array of x already
    }
    procedure clearArrayProperty(prop: IInterface);
    var
        i, num: integer;

    begin
        num := ElementCount(prop);
        for i:=0 to num-1 do begin
            RemoveElement(prop, 0);
        end;
    end;

    {
        Reset given script property, if it's set
    }
    procedure clearScriptProp(script: IInterface; propName: string);
    var
        rawProp : IInterface;
    begin
        rawProp := getRawScriptProp(script, propName);
        if(not assigned(rawProp)) then exit;

        clearProperty(rawProp);
    end;

    procedure clearScriptProperty(script: IInterface; propName: string);
    begin
        clearScriptProp(script, propName);
    end;

    {
        Get a struct member. If not set, return given defaultValue instead
    }
    function getStructMemberDefault(struct: IInterface; name: String; defaultValue: variant): variant;
    var
        member: IInterface;
    begin
        member := getRawStructMember(struct, name);

        Result := getValueAsVariant(member, defaultValue);
    end;

    {
        Get a struct member. Returns nil if it isn't set.
    }
    function getStructMember(struct: IInterface; name: String): variant;
    begin
        Result := getStructMemberDefault(struct, name, nil);
    end;

    {
        Appends an object to an "Array of Object" property value
    }
    procedure appendObjectToProperty(prop: IInterface; newObject: IInterface);
    var
        newEntry, propValue: IInterface;
    begin
        propValue := ElementByPath(prop, 'Value\Array of Object');
        if(not assigned(propValue)) then begin
            propValue := prop; // assume we were given the array of object already
        end;

        newEntry := ElementAssign(propValue, HighInteger, nil, false);

        SetLinksTo(ElementByPath(newEntry, 'Object v2\FormID'), newObject);
    end;

    {
        Appends an object to an "Array of Object" property value, unless it already exists
    }
    procedure ensurePropertyHasObject(prop: IInterface; newObject: IInterface);
    var
        newEntry, propValue, curEntry: IInterface;
        i: integer;
    begin
        propValue := ElementByPath(prop, 'Value\Array of Object');
        if(not assigned(propValue)) then begin
            propValue := prop; // assume we were given the array of object already
        end;

        for i:=0 to ElementCount(propValue)-1 do begin
            curEntry := ElementByIndex(propValue, i);
            if(IsSameForm(newObject, PathLinksTo(curEntry, 'Object v2\FormID'))) then begin
                exit;
            end;
        end;

        newEntry := ElementAssign(propValue, HighInteger, nil, false);

        SetPathLinksTo(newEntry, 'Object v2\FormID', newObject);
    end;

    procedure removeObjectFromProperty(prop: IInterface; objectToRemove: IInterface);
    var
        newEntry, propValue, curEntry: IInterface;
        i: integer;
    begin
        propValue := ElementByPath(prop, 'Value\Array of Object');
        if(not assigned(propValue)) then begin
            propValue := prop; // assume we were given the array of object already
        end;

        for i:=0 to ElementCount(propValue)-1 do begin
            curEntry := ElementByIndex(propValue, i);
            if(IsSameForm(objectToRemove, PathLinksTo(curEntry, 'Object v2\FormID'))) then begin
                RemoveElement(propValue, i);
            end;
        end;
    end;

    {
        Gets an object from an "Array of Object" property value at the given index
    }
    function getObjectFromProperty(prop: IInterface; i: integer): IInterface;
    var
        propValue, curStuff: IInterface;
    begin
        propValue := ElementByPath(prop, 'Value\Array of Object');
        if(not assigned(propValue)) then begin
            propValue := prop; // assume we were given the array of object already
        end;

        curStuff := ElementByPath(ElementByIndex(propValue, i), 'Object v2\FormID');
        Result := LinksTo(curStuff);
    end;

    function getPropertyArrayLength(prop: IInterface): integer;
    var
        typeStr: string;
        propValue, curStuff: IInterface;
    begin
        Result := 0;
        typeStr := GetElementEditValues(prop, 'Type');
        if(typeStr <> '') then begin
            if(not strStartsWith(typeStr, 'Array of')) then exit;

            propValue := ElementByPath(prop, 'Value\'+typeStr);
            if(not assigned(propValue)) then begin
                exit;
            end;
        end else begin
            propValue := prop;
        end;

        Result := ElementCount(propValue);
    end;

    {
        Removes an entry from an array property at the given index.
    }
    procedure removeEntryFromProperty(prop: IInterface; i: integer);
    var
        propValue, curStuff: IInterface;
        typeStr: string;
    begin
        typeStr := GetElementEditValues(prop, 'Type');
        if(typeStr <> '') then begin
            if(not strStartsWith(typeStr, 'Array of')) then exit;
            propValue := ElementByPath(prop, 'Value\' + typeStr);
        end else begin
            propValue := prop;
        end;

        RemoveElement(propValue, i);

    end;

    {
        Gets something from an "Array of x" property value at the given index
    }
    function getValueFromProperty(prop: IInterface; i: integer): variant;
    begin
        Result := getValueFromPropertyDefault(prop, i, nil);
    end;

    function getValueFromPropertyDefault(prop: IInterface; i: integer; defaultValue: variant): variant;
    var
        typeStr, arrayType: string;
        curElem: IInterface;
    begin
        Result := defaultValue;
        typeStr := GetElementEditValues(prop, 'Type');
        if(not strStartsWith(typeStr, 'Array of')) then exit;
        arrayType := copy(typeStr, 10, length(typeStr));

        curElem := ElementByIndex(propValue, i);

        // easy types
        if(arrayType = 'String') then begin
            Result := GetEditValue(curElem);
            exit;
        end;

        if(arrayType = 'Int32') then begin
            Result := StrToInt(GetEditValue(curElem));
            exit;
        end;

        if(arrayType = 'Float') then begin
            Result := StrToFloat(GetEditValue(curElem));
            exit;
        end;

        if(arrayType = 'Bool') then begin
            Result := StrToBool(GetEditValue(curElem));
            exit;
        end;

        // struct
        if(arrayType = 'Struct') then begin
            Result := curElem;
            exit;
        end;

        // Object
        if(arrayType = 'Object') then begin
            Result := pathLinksTo(curElem, 'Object v2\FormID');
            exit;
        end;
    end;

    {
        Checks whenever an "Array of Object" has a certain object in it
    }
    function hasObjectInProperty(prop: IInterface; entry: IInterface): boolean;
    var
        propValue, curEntry, curStuff: IInterface;
        i: integer;
    begin
        propValue := ElementByPath(prop, 'Value\Array of Object');
        if(not assigned(propValue)) then begin
            propValue := prop; // assume we were given the array of object already
        end;



        for i:=0 to ElementCount(propValue)-1 do begin
            curStuff := ElementByPath(ElementByIndex(propValue, i), 'Object v2\FormID');
            curEntry := LinksTo(curStuff);
            if(isSameForm(entry, curEntry)) then begin
                Result := true;
                exit;
            end;
        end;

        Result := false;
    end;

    {
        Appends an empty struct to an "Array of Struct" property value
    }
    function appendStructToProperty(prop: IInterface): IInterface;
    var
        newEntry, propValue: IInterface;
    begin
        propValue := ElementByPath(prop, 'Value\Array of Struct');
        if(not assigned(propValue)) then begin
            propValue := prop; // assume we were given the array of struct already
        end;

        Result := ElementAssign(propValue, HighInteger, nil, false);
    end;

    {

        Decodes the two-byte string edit value of VISI or PCMB into a numeric previsibine timestamp.
    }
    function decodeHexTimestampString(ts: string): cardinal;
    var
        sl: TStringList;
    begin
        Result := 0;
        if(ts = '') then exit;
        sl := TStringList.Create;
        sl.DelimitedText := ts;
        Result := StrToInt64('$' + sl[1] + sl[0]);
        sl.free();
    end;

    {
        Encodes a numeric previsibine timestamp into a string containing two bytes for writing into the cell's edit value of VISI or PCMB.
    }
    function encodeHexTimestampString(ts: cardinal): string;
    var
        byte1, byte2: cardinal;
    begin
        byte1 := (ts and $FF00) shr 8;
        byte2 := (ts and $FF);

        Result := IntToHex(byte2, 2)+' '+IntToHex(byte1, 2);
    end;

    {
        Encodes day, month, year into a previs timestamp
    }
    function dateToTimestamp(day, month, year: cardinal): cardinal;
    begin
        // Day      0000000000011111 = $1F
        // Month    0000000111100000 = $1E0  // shl 5
        // Year     1111111000000000 = $FE00 // shl 9 (starts at 2000)
        year := year - 2000;

        Result := (day and $1F) or ((month shl 5) and $1E0) or ((year shl 9) and $FE00);
    end;

    {
        Converts a previsibine timestamp into a YYYY-MM-DD formatted string.
    }
    function timestampToDate(timestamp: cardinal): string;
    var
        day, month, year: cardinal;
        dayStr, monthStr: string;
    begin
        day := (timestamp and $1F);
        month := (timestamp and $1E0) shr 5;
        year := ((timestamp and $FE00) shr 9) + 2000;

        dayStr := IntToStr(day);
        if(day < 10) then begin
            dayStr := '0'+dayStr;
        end;
        monthStr := IntToStr(month);
        if(month < 10) then begin
            monthStr := '0'+monthStr;
        end;

        Result := IntToStr(year)+'-'+monthStr+'-'+dayStr;
    end;

    {
        If some mod uses "injected recods", aka: an override without the master actually existing (looking at you, UFO4P), this should get the intended target file
    }
    function getInjectedRecordTarget(elem: IInterface): IInterface;
    var
        sourceFile: IInterface;
        fileLoadOrderMain, fileLoadOrderEsl, curFormID, mainLoadOrder, eslLoadOrder: cardinal;
    begin
        Result := nil;
        if(not IsInjected(elem)) then exit;
        // now figure out what this is injected into
        // ugh this is a horrible mess
        // it seems like my FileBy* functions are also slow AF

        curFormID := GetLoadOrderFormID(elem);// FormID(elem) so this returns the FormID from the parent file's PoV, it seems
        mainLoadOrder := (curFormID and $FF000000) shr 24;
        if(mainLoadOrder = $FE) then begin
            // an ESL is targeted
            eslLoadOrder := (curFormID and $00FFF000) shr 12;
            Result := FileByLightLoadOrder(eslLoadOrder);
        end else begin
            Result := FileByRealLoadOrder(mainLoadOrder);
        end;
    end;

    procedure ReportRequiredMastersFull_Recursive(e: IInterface; list, loopPreventer: TStringList);
    var
        masters: TStringList;
        i: integer;
        child, maybeLinksTo: IInterface;
        curKey: string;
    begin
        curKey := FormToStr(e);
        if (loopPreventer.indexOf(curKey) > -1) then exit;
        loopPreventer.add(curKey);

        // first, do the vanilla thing
        ReportRequiredMasters(e, list, true, true);

        // now, do it recursively
        for i := 0 to ElementCount(e)-1 do begin
            child := ElementByIndex(e, i);
            maybeLinksTo := LinksTo(child);
            if(assigned(maybeLinksTo)) then begin
                ReportRequiredMastersFull_Recursive(maybeLinksTo, list, loopPreventer);
            end;
        end;

    end;

    {
        Similar to ReportRequiredMasters, but should actually report every single master required, even for REFRs
    }
    function ReportRequiredMastersFull(e: IInterface): TStringList;
    var
        loopPreventer: TStringList;
        i: integer;
        child, maybeLinksTo: IInterface;

    begin
        Result := TStringList.create;
        Result.Duplicates := dupIgnore;
        Result.CaseSensitive := false;
        Result.Sorted := true; // important, or dupIgnore won't work

        // keep track of everything we recurse into, to prevent endless loops
        // because there are indeed circular links
        loopPreventer := TStringList.create;
        loopPreventer.Duplicates := dupIgnore;
        loopPreventer.CaseSensitive := false;
        loopPreventer.Sorted := true;

        ReportRequiredMastersFull_Recursive(e, Result, loopPreventer);

        loopPreventer.free();
    end;

    {
        Checks whenever fileToCheck or any of it's masters have masterToCheck as their master
    }
    function HasMasterFull(fileToCheck: IInterface; masterToCheck: string): boolean;
    var
        i, cntMinOne: integer;
    begin
        // first, directly
        if(HasMaster(fileToCheck, masterToCheck)) then begin
            Result := true;
            exit;
        end;

        cntMinOne := MasterCount(fileToCheck)-1;

        for i:=0 to cntMinOne do begin
            if(HasMasterFull(MasterByIndex(fileToCheck, i), masterToCheck)) then begin
                Result := true;
                exit;
            end;
        end;

        Result := false;
    end;

    {
        Like AddMasterIfMissing, but will do nothing and return false if adding the master would cause a circular dependency.
    }
    function AddMasterIfMissing_Safe(toFile: IInterface; newMaster: string): boolean;
    var
        newMasterFile: IInterface;
    begin
        Result := false;
        newMasterFile := findFile(newMaster);
        if(not assigned(newMasterFile)) then begin
            AddMessage('ERROR: cannot add '+newMaster+' as master to '+GetFileName(toFile)+', because '+newMaster+' doesn''t exist');
            exit;
        end;

        // now, what do we do? newMasterFile must absolutely not have toFile as master
        if(HasMasterFull(newMasterFile, GetFileName(toFile))) then begin
            AddMessage('ERROR: cannot add '+newMaster+' as master to '+GetFileName(toFile)+', because that would cause a circular dependency!');
            exit;
        end;

        AddMasterIfMissing(toFile, newMaster);
        Result := true;
    end;

    {
        Gets the file of fromElement, and adds it and all it's masters to toFile.
        fromElement can be either an element or a file.
    }
    function addRequiredMastersSilent_Single(fromElement, toFile: IInterface): boolean;
    var
        masters: TStringList;
        i: integer;
        toFileName: string;
        fromElemFile: IInterface;
    begin
        Result := true;
        toFileName := GetFileName(toFile);
        fromElemFile := GetFile(fromElement);

        if (not FilesEqual(fromElemFile, toFile)) then begin
            if(not AddMasterIfMissing_Safe(toFile, GetFileName(fromElemFile))) then begin
                Result := false;
            end;
        end;

        masters := ReportRequiredMastersFull(fromElement);
        for i:=0 to masters.count-1 do begin
            if(toFileName <> masters[i]) then begin
                if(not AddMasterIfMissing_Safe(toFile, masters[i])) then begin
                    Result := false;
                    masters.free();
                    exit;
                end;
            end;
        end;
        masters.free();
    end;


    {
        Like AddRequiredElementMasters, but just adds them, without showing any confirmation box
    }
    procedure addRequiredMastersSilent(fromElement, toFile: IInterface);
    var
        curMaster, injectedMaster: IInterface;
    begin
        if(not assigned(fromElement)) then begin
            AddMessage('WARNING: addRequiredMastersSilent was called with a none fromElement');
            exit;
        end;
        if(not isMaster(fromElement)) then begin
            curMaster := Master(fromElement);
            if(not addRequiredMastersSilent_Single(curMaster, toFile)) then begin
                raise Exception.Create('ERROR: Cannot add required masters for '+FullPath(fromElement)+' to '+GetFileName(toFile));
                //AddMessage('ERROR: Cannot add required masters for '+DisplayName(fromElement)+' to '+GetFileName(toFile));
                //exit;
            end;
        end;

        injectedMaster := getInjectedRecordTarget(fromElement);
        if(assigned(injectedMaster)) then begin
            if(not addRequiredMastersSilent_Single(injectedMaster, toFile)) then begin
                raise Exception.Create('ERROR: Cannot add required masters for '+FullPath(fromElement)+' to '+GetFileName(toFile));
                //AddMessage('ERROR: Cannot add required masters for '+DisplayName(fromElement)+' to '+GetFileName(toFile));
                //exit;
            end;
        end;

        if(not addRequiredMastersSilent_Single(fromElement, toFile)) then begin
            raise Exception.Create('ERROR: Cannot add required masters for '+FullPath(fromElement)+' to '+GetFileName(toFile));
            //AddMessage('ERROR: Cannot add required masters for '+DisplayName(fromElement)+' to '+GetFileName(toFile));
            //exit;
        end;
    end;

    function getExistingElementOverride(sourceElem: IInterface; targetFile: IwbFile): IInterface;
    var
        masterElem, curOverride: IINterface;
        numOverrides, i: integer;
        targetFileName: string;
    begin
        Result := nil;

        masterElem := MasterOrSelf(sourceElem);
        targetFileName := GetFileName(targetFile);

        // important failsafe
        if(FilesEqual(targetFile,  GetFile(masterElem))) then begin
            Result := sourceElem;
            exit;
        end;

        numOverrides := OverrideCount(masterElem);

        for i:=0 to numOverrides-1 do begin
            curOverride := OverrideByIndex(masterElem, i);

            if (FilesEqual(GetFile(curOverride), targetFile)) then begin
                Result := curOverride;
                exit;
            end;
        end;
    end;

    function getWinningOverrideBefore(sourceElem: IInterface; notInThisFile: IwbFile): IInterface;
    var
        masterElem, curOverride, prevOverride: IINterface;
        numOverrides, i: integer;

    begin

        masterElem := MasterOrSelf(sourceElem);

        Result := masterElem;

        if(FilesEqual(notInThisFile,  GetFile(masterElem))) then begin
            Result := nil;
            exit;
        end;

        numOverrides := OverrideCount(masterElem);
        prevOverride := masterElem;
        for i:=0 to numOverrides-1 do begin
            curOverride := OverrideByIndex(masterElem, i);
            Result := prevOverride;

            if (FilesEqual(GetFile(curOverride), notInThisFile)) then begin
                exit;
            end;
            prevOverride := curOverride;
        end;
    end;


    function getExistingElementOverrideOrClosest(sourceElem: IInterface; targetFile: IwbFile): IInterface;
    var
        masterElem, curOverride: IINterface;
        numOverrides, i: integer;
        targetFileName: string;
    begin

        masterElem := MasterOrSelf(sourceElem);
        targetFileName := GetFileName(targetFile);
        Result := masterElem;

        // important failsafe
        if(FilesEqual(targetFile,  GetFile(masterElem))) then begin
            Result := sourceElem;
            exit;
        end;

        numOverrides := OverrideCount(masterElem);

        for i:=0 to numOverrides-1 do begin
            curOverride := OverrideByIndex(masterElem, i);
            Result := curOverride;

            if (FilesEqual(GetFile(curOverride), targetFile)) then begin
                exit;
            end;
        end;
    end;

    function createElementOverride(sourceElem: IInterface; targetFile: IwbFile): IInterface;
    var
        existingOverride: IInterface;
    begin
        existingOverride := getExistingElementOverride(sourceElem, targetFile);
        if(equals(existingOverride, sourceElem)) then begin
            Result := existingOverride;
            exit;
        end;

        if(assigned(existingOverride)) then begin
            Remove(existingOverride);
        end;

        addRequiredMastersSilent(sourceElem, targetFile);
        Result := wbCopyElementToFile(sourceElem, targetFile, False, True);
    end;

    function getOrCreateElementOverride(sourceElem: IInterface; targetFile: IwbFile): IInterface;
    var
        existingOverride: IInterface;
    begin
        existingOverride := getExistingElementOverride(sourceElem, targetFile);

        if(assigned(existingOverride)) then begin
            Result := existingOverride;
            exit;
        end;

        addRequiredMastersSilent(sourceElem, targetFile);
        Result := wbCopyElementToFile(sourceElem, targetFile, False, True);
    end;

    //GUI function
    {
        This should escape characters which have special meaning when used in a UI
    }
    function escapeString(str: string): string;
    begin
        Result := StringReplace(str, '&', '&&', [rfReplaceAll]);
    end;

	{
		Removes all strings except letters, numbers, _ and -
	}
	function cleanStringForEditorID(str: string): string;
	var
        regex: TPerlRegEx;
    begin
        Result := '';
        regex  := TPerlRegEx.Create();
        try
            regex.RegEx := '[^a-zA-Z0-9_-]+';
            regex.Subject := trim(str);
            regex.Replacement := '';
            regex.ReplaceAll();
            Result := regex.Subject;
        finally
            RegEx.Free;
        end;
	end;

    function CreateDialog(caption: String; width, height: Integer): TForm;
    var
        frm: TForm;
    begin
        frm := TForm.Create(nil);
        frm.BorderStyle := bsDialog;
        frm.Height := height;
        frm.Width := width;
        frm.Position := poScreenCenter;
        frm.Caption := escapeString(caption);

        Result := frm;
    end;

    function CreateButton(frm: TForm; left: Integer; top: Integer; caption: String): TButton;
    begin
        Result := TButton.Create(frm);
		Result.Width := Length(caption) * 10;
        Result.Parent := frm;
        Result.Left := left;
        Result.Top := top;
        Result.Caption := escapeString(caption);
    end;

    function CreateLabel(frm: TForm; left, top: Integer; text: String): TLabel;
    begin
        Result := TLabel.Create(frm);
        Result.Parent := frm;
        Result.Left := left;
        Result.Top := top;
        Result.Caption := escapeString(text);
    end;

    function CreateCheckbox(frm: TForm; left, top: Integer; text: String): TCheckBox;
    begin
        Result := TCheckBox.Create(frm);
        Result.Parent := frm;
        Result.Left := left;
        Result.Top := top;
        Result.Caption := escapeString(text);
        Result.Width := Length(text) * 10;
    end;

	function CreateRadioGroup(frm: TForm; left, top, width, height: Integer; caption: String; items: TStringList): TRadioGroup;
	begin
		Result := TRadioGroup.Create(frm);
        Result.Parent := frm;
        Result.Left := left;
        Result.Top := top;
        Result.Width := width;
        Result.Height := height;
        Result.Caption := caption;

        if(items <> nil) then begin
            Result.items := items;
        end;
	end;

    function CreateInput(frm: TForm; left, top: Integer; text: String): TEdit;
    begin
        Result := TEdit.Create(frm);
        Result.Parent := frm;
        Result.Left := left;
        Result.Top := top;
        Result.Text := escapeString(text);
    end;

    function CreateMultilineInput(frm: TForm; left, top, width, height: Integer; text: String): TCustomMemo;
    begin
        Result := TCustomMemo.Create(frm);
        Result.Parent := frm;
        Result.Left := left;
        Result.Top := top;
        Result.Width := width;
        Result.height := height;
        Result.Text := escapeString(text);
    end;

    function CreateLabelledInput(frm: TForm; left, top, width, height: Integer; caption, text: String): TLabeledEdit;
    begin
        Result := TLabeledEdit.Create(frm);
        Result.Parent := frm;

        Result.Left := left;
        Result.Top := top;
        Result.Width := width;
        Result.LabelPosition := lpAbove;
        Result.EditLabel.Caption := caption;
        Result.Text := text;
    end;

    function CreateGroup(frm: TForm; left: Integer; top: Integer; width: Integer; height: Integer; caption: String): TGroupBox;
    begin
        Result := TGroupBox.Create(frm);
		Result.Parent := frm;
		Result.Top := top;
		Result.Left := left;
		Result.Width := width;
		Result.Height := height;
		Result.Caption := escapeString(caption);
		Result.ClientWidth := width-10;//274; // maybe width -10
		Result.ClientHeight := height+9;//85; // maybe height +9
    end;

    function CreateComboBox(frm: TForm; left: Integer; top: Integer; width: Integer; items: TStringList): TComboBox;
    begin
        Result := TComboBox.Create(frm);
        Result.Parent := frm;
        Result.Left := left;
        Result.Top := top;
        Result.Width := width;

        if(items <> nil) then begin
            Result.items := items;
        end;
    end;

	function CreateListBox(frm: TForm; left: Integer; top: Integer; width: Integer; height: Integer; items: TStringList): TListBox;
	begin
		Result := TListBox.Create(frm);
        Result.Parent := frm;
        Result.Left := left;
        Result.Top := top;
        Result.Width := width;
        Result.Height := height;

        if(items <> nil) then begin
            Result.items := items;
        end;
	end;

    function ShowFileSelectDialog(caption: string): IInterface;
    begin
        Result := ShowFileSelectDialogExtended(caption, true, true, nil);
    end;

    function ShowFileSelectDialogExtended(caption: string; prependNewFileEntry, skipNotEditable: boolean; preselectedFile: IInterface): IInterface;
    var
        frm: TForm;
        targetFileBox: TComboBox;
        btnOk, btnCancel: TButton;
        resultCode: cardinal;
        newFileName: string;
    begin
        Result := nil;
        frm := CreateDialog('Select File', 400, 150);

        CreateLabel(frm, 20, 10, caption);
        targetFileBox := CreateFileSelectDropdownExtended(frm, 20, 30, 280, preselectedFile, prependNewFileEntry, skipNotEditable);

        btnOk := CreateButton(frm, 50, 60, '    OK    ');
        btnCancel := CreateButton(frm, 230, 60, '  Cancel  ');

        btnCancel.ModalResult := mrCancel;
        btnOk.ModalResult := mrOk;

        resultCode := frm.showModal();
        if(resultCode <> mrOk) then begin
            frm.free();
            exit;
        end;

        if(prependNewFileEntry and targetFileBox.ItemIndex = 0) then begin
            Result := AddNewFile();
        end else begin
            newFileName := targetFileBox.Items[targetFileBox.ItemIndex];
            Result := FindFile(newFileName);
        end;

        frm.free();
    end;

    function CreateFileSelectDropdown(frm: TForm; left: Integer; top: Integer; width: Integer; preselectedFile: IInterface; prependNewFileEntry: boolean): TComboBox;
    var
        btnOk, btnCancel: TButton;
        i, selIndex, fileIndex: integer;
        curFileName: string;
        curFile: IInteerface;
    begin
        Result := CreateFileSelectDropdownExtended(frm, left, top, width, preselectedFile, prependNewFileEntry, false);
    end;

    function CreateFileSelectDropdownExtended(frm: TForm; left: Integer; top: Integer; width: Integer; preselectedFile: IInterface; prependNewFileEntry, skipNotEditable: boolean): TComboBox;
    var
        btnOk, btnCancel: TButton;
        i, selIndex, fileIndex: integer;
        curFileName: string;
        curFile: IInteerface;
    begin
        Result := CreateComboBox(frm, left, top, width, nil);

        if(prependNewFileEntry) then begin
            Result.Items.Add('-- CREATE NEW FILE --');
        end;

        selIndex := 0;
        for i := 0 to FileCount - 1 do begin
            curFile := FileByIndex(i);

            curFileName := GetFileName(curFile);
            if(skipNotEditable) then begin
                if(not isEditable(curFile)) then begin
                    continue;
                end;
            end;

            fileIndex := Result.Items.Add(curFileName);

            if(assigned(preselectedFile)) then begin
                if(FilesEqual(curFile, preselectedFile)) then begin
                    selIndex := fileIndex;
                end;
            end;
        end;
        Result.ItemIndex := selIndex;
        Result.Style := csDropDownList;
    end;

    {
        Shows a dialog with input fields for x, y and z.
        If cancelled, will return nil.
        Otherwise, will return a TStringList containinig the x, y and z values as strings in 0, 1, and 2 respectively.

        You probably should call .free on the result from this function

        @param string caption   caption of the dialog
        @param string text      text to display on the dialog
        @param float x          default values to pre-fill the inputs with
        @param float y
        @param float z
    }
    function ShowVectorInput(caption, text: string; x, y, z: float): TStringList;
    var
        frm: TForm;
        btnOkay, btnCancel: TButton;

        resultCode: Integer;
        inputX, inputY, inputZ: TEdit;
    begin
        Result := nil;


        frm := CreateDialog(caption, 400, 200);

        CreateLabel(frm, 10, 6, text);
        //CreateLabel(frm, 10, 20, 'EDID: '+curEdid);

        CreateLabel(frm, 10, 45, 'X');
        CreateLabel(frm, 10, 75, 'Y');
        CreateLabel(frm, 10, 105, 'Z');

        inputX := CreateInput(frm, 20, 43, FloatToStr(x));
        inputY := CreateInput(frm, 20, 73, FloatToStr(y));
        inputZ := CreateInput(frm, 20, 103, FloatToStr(z));

        btnOkay := CreateButton(frm, 110, 140, 'OK');
        btnOkay.ModalResult := mrYes;
        btnOkay.Default := true;

        btnCancel := CreateButton(frm, 200, 140, 'Cancel');
        btnCancel.ModalResult := mrCancel;

        resultCode := frm.ShowModal;

        if(resultCode <> mrYes) then begin
            Result := nil;
            frm.free();
            exit;
        end;

        Result := TStringList.create;

        Result.add(inputX.Text);
        Result.add(inputY.Text);
        Result.add(inputZ.Text);


        frm.free();
    end;

    {
        Creates a TOpenDialog for opening a file. Doesn't show it yet.
        WARNING: xEdit doesn't actually support default parameters, they are just there to show you what to pass if you don't know/don't care.

        @param string title             This will be displayed in the dialog's title bar, something like 'Select file to import' or just 'Open File'
        @param string filter            Can be used to specify which files can be opened. The syntax is rather weird:
                                            - To specify a filetype, it's '<description text>|<filter>', for example: 'Text files|*.txt'.
                                            - If the filetype can have more than one extension, they can be separated by a ';', for example: 'Plugin Files|*.esp;*.esm;*.esl'.
                                            - To use more than one filters, you can specify several filetypes as above, separated by |, for example:  'Text files|*.txt|Plugin Files|*.esp;*.esm;*.esl'.
                                              Yes, pipe separates both the description and filters, and filetypes. It's not my fault, it's just Pascal...
                                            - To allow any file whatsoever, pass empty string.
                                            For more infos, see http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Dialogs_TOpenDialog_Filter.html
        @param string initialDir        Path where the open dialog will start. If empty string is passed, it will remember the directory you selected a file before and start with that.
        @param boolean mustExist        If false, it will allow you to type any filename and press "Open", whenever it exists or not.

        @return                         An instance of TOpenDialog. You must call .free on it after you are done.
    }
    function CreateOpenFileDialog(title: string; filter: string = ''; initialDir: string = ''; mustExist:boolean = true): TOpenDialog;
    var
        objFile: TOpenDialog;
    begin
        objFile := TOpenDialog.Create(nil);
        Result := nil;

        objFile.Title := title;
        if(mustExist) then objFile.Options := [ofFileMustExist];

        if(initialDir <> '') then begin
            objFile.InitialDir  := initialDir;
        end;

        if(filter <> '') then begin
            objFile.Filter := filter;
            objFile.FilterIndex := 1;
        end;
        Result := objFile;

    end;

    {
        Creates a TSaveDialog for saving to a file. Doesn't show it yet.
        WARNING: xEdit doesn't actually support default parameters, they are just there to show you what to pass if you don't know/don't care.

        Parameters are identical to CreateOpenFileDialog

        @return     An instance of TSaveDialog. You must call .free on it after you are done.
    }
    function CreateSaveFileDialog(title: string; filter: string = ''; initialDir: string = ''): TSaveDialog;
    var
        objFile: TSaveDialog;
    begin
        objFile := TSaveDialog.Create(nil);
        Result := nil;

        objFile.Title := title;
        objFile.Options := objFile.Options + [ofOverwritePrompt];

        if(initialDir <> '') then begin
            objFile.InitialDir  := initialDir;
        end;

        if(filter <> '') then begin
            objFile.Filter := filter;
            objFile.FilterIndex := 1;
        end;
        Result := objFile;
    end;

    {
        A shortcut for showing an Open File dialog. The parameters title and filter are identical to CreateOpenFileDialog, see that for description.
        Returns the path of the selected file, or empty string if cancelled
    }
    function ShowOpenFileDialog(title: string; filter:string = ''): string;
    var
        objFile: TOpenDialog;
    begin
        objFile := CreateOpenFileDialog(title, filter, '', true);
        Result := '';
        try
            if objFile.Execute then begin
                Result := objFile.FileName;
            end;
        finally
            objFile.free;
        end;
    end;

    {
        A shortcut for showing a Save File dialog. The parameters title and filter are identical to CreateOpenFileDialog, see that for description.
        Returns the path of the selected file, or empty string if cancelled
    }
    function ShowSaveFileDialog(title: string; filter:string = ''): string;
    var
        objFile: TSaveDialog;
    begin
        objFile := CreateSaveFileDialog(title, filter, '');
        Result := '';
        try
            if objFile.Execute then begin
                Result := objFile.FileName;
            end;
        finally
            objFile.free;
        end;
    end;

    // === JSON FUNCTIONS ===
    // merged in from an old defunct JSON library, for an old defunct project
    function jsonTypeToString(t: integer): string;
    begin
        case t of
            JSON_TYPE_NONE:     Result := 'none';
            JSON_TYPE_STRING:   Result := 'string';
            JSON_TYPE_INT:      Result := 'int';
            JSON_TYPE_LONG:     Result := 'long';
            JSON_TYPE_ULONG:    Result := 'ulong';
            JSON_TYPE_FLOAT:    Result := 'float';
            JSON_TYPE_DATETIME: Result := 'datetime';
            JSON_TYPE_BOOL:     Result := 'bool';
            JSON_TYPE_ARRAY:    Result := 'array';
            JSON_TYPE_OBJECT:   Result := 'object';
        end;
    end;

    // prefix-based helpers
    function getJsonKeyByPrefix(obj: TJsonObject; substr: string): string;
    var
        i: integer;
        curName: string;
    begin
        Result := substr;
        for i:=0 to obj.count-1 do begin
            curName := obj.Names[i];
            if(strStartsWith(curName, substr)) then begin
                Result := curName;
                exit;
            end;
        end;
    end;

    function getTypeAtPrefixPath(src: TJsonObject; objPath: string): int;
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin
        Result := JSON_TYPE_NONE;
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := getJsonKeyByPrefix(src, tmpList[i]);
            if(i < lastIndex) then begin
                if(pathObj.Types[curSubpath] <> JSON_TYPE_OBJECT) then begin
                    tmpList.free();
                    exit;
                end;
                pathObj := pathObj.O[curSubpath];
            end else begin
                Result := pathObj.Types[curSubpath];
            end;
        end;

        tmpList.free();
    end;

    function getValueAtPrefixPath(src: TJsonObject; objPath: string; jsonType: integer; default: variant): variant;
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin
        Result := default;
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := getJsonKeyByPrefix(src, tmpList[i]);
            if(pathObj.Types[curSubpath] = JSON_TYPE_NONE) then begin
                tmpList.free();
                exit;
            end;

            if(i < lastIndex) then begin
                pathObj := pathObj.O[curSubpath];
            end else begin
                if(jsonType = -1) then begin
                    jsonType := pathObj.Types[curSubpath];
                end;
                case jsonType of
                    JSON_TYPE_STRING:   Result := pathObj.S[curSubpath];
                    JSON_TYPE_INT:      Result := pathObj.I[curSubpath];
                    JSON_TYPE_LONG:     Result := pathObj.L[curSubpath];
                    JSON_TYPE_ULONG:    Result := pathObj.U[curSubpath];
                    JSON_TYPE_FLOAT:    Result := pathObj.F[curSubpath];
                    JSON_TYPE_DATETIME: Result := pathObj.D[curSubpath];
                    JSON_TYPE_BOOL:     Result := pathObj.B[curSubpath];
                    JSON_TYPE_ARRAY:    Result := pathObj.A[curSubpath];
                    JSON_TYPE_OBJECT:   Result := pathObj.O[curSubpath];
                end;

            end;
        end;

        tmpList.free();
    end;

    procedure setValueAtPrefixPath(src: TJsonObject; objPath: string; jsonType: integer; value: variant);
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin

        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := getJsonKeyByPrefix(src, tmpList[i]);

            if(i < lastIndex) then begin
                pathObj := pathObj.O[curSubpath];
            end else begin
                case jsonType of
                    JSON_TYPE_STRING:   pathObj.S[curSubpath] := value;
                    JSON_TYPE_INT:      pathObj.I[curSubpath] := value;
                    JSON_TYPE_LONG:     pathObj.L[curSubpath] := value;
                    JSON_TYPE_ULONG:    pathObj.U[curSubpath] := value;
                    JSON_TYPE_FLOAT:    pathObj.F[curSubpath] := value;
                    JSON_TYPE_DATETIME: pathObj.D[curSubpath] := value;
                    JSON_TYPE_BOOL:     pathObj.B[curSubpath] := value;
                    JSON_TYPE_ARRAY:    pathObj.A[curSubpath] := cloneJsonArray(value);
                    JSON_TYPE_OBJECT:   pathObj.O[curSubpath] := cloneJsonObject(value);
                end;
            end;
        end;

        tmpList.free();
    end;

    // element helpers
    procedure elemToJsonArrayRecursive(e: IInterface; resultSet: TJsonArray);
    var
        i: integer;
        child: IInterface;
        curName, curEditVal: string;
        curNativeVal: variant;
        curArr: TJsonArray;
        curObj: TJsonObject;
        curLinksTo: IInterface;
    begin
        if(not assigned(e)) then exit;
        for i:=0 to ElementCount(e)-1 do begin
            child := ElementByIndex(e, i);
            curEditVal := GetEditValue(child);
            if(curEditVal <> '') then begin
                curLinksTo := LinksTo(child);
                if(assigned(curLinksTo)) then begin
                    resultSet.add(FormToAbsStr(curLinksTo));
                end else begin
                    curNativeVal := GetNativeValue(child);
                    resultSet.add(curNativeVal);
                end;
            end else begin
                if(isSubrecordArray(child)) then begin
                    curArr := resultSet.addArray();
                    elemToJsonArrayRecursive(child, curArr);
                end else begin
                    if(isSubrecordScalar(child)) then begin
                        resultSet.add('');
                    end else begin;
                        curObj := resultSet.addObject();
                        elemToJsonObjectRecursive(child, curObj);
                    end;
                end;
            end;
        end;
    end;

    procedure elemToJsonObjectRecursive(e: IInterface; resultSet: TJsonObject);
    var
        i: Integer;
        child: IInterface;
        curName, curEditVal: string;
        curArr: TJsonArray;
        curObj: TJsonObject;
        curNativeVal: variant;
        curLinksTo: IInterface;
    begin
        if(not assigned(e)) then exit;
        for i := 0 to ElementCount(e)-1 do begin
            child := ElementByIndex(e, i);
            curName := Name(child);
            curEditVal := GetEditValue(child);
            if(curEditVal <> '') then begin
                curLinksTo := LinksTo(child);
                if(assigned(curLinksTo)) then begin
                    resultSet.S[curName] := FormToAbsStr(curLinksTo);
                end else begin
                    curNativeVal := GetNativeValue(child);
                    setJsonObjectValueVariant(resultSet, curName, curNativeVal, curEditVal);
                end;
            end else begin
                if(isSubrecordArray(child)) then begin
                    curArr := TJsonArray.create;
                    elemToJsonArrayRecursive(child, curArr);
                    resultSet.A[curName] := curArr;
                end else begin
                    if(isSubrecordScalar(child)) then begin
                        resultSet.S[curName] := '';
                    end else begin
                        elemToJsonObjectRecursive(child, resultSet.O[curName]);
                    end;
                end;
            end;
        end;
    end;

    {
        Returns a JSON representing the given (sub)element's structure
    }
    function elemToJson(e: IInterface): TJsonObject;
    begin
        Result := TJsonObject.create;
        elemToJsonObjectRecursive(e, Result);
    end;

    procedure removeEqualArrayEntries(check, compareTo: TJsonArray);
    var
        i, curType: integer;
        curName: string;
    begin
        for i:=0 to check.count-1 do begin
            if(i>=compareTo.count) then exit;
            curType := check.Types[i];
            case curType of
                JSON_TYPE_ARRAY:
                    begin
                        removeEqualArrayEntries(check.A[i], compareTo.A[i]);
                    end;
                JSON_TYPE_OBJECT:
                    begin
                        removeEqualObjectEntries(check.O[i], compareTo.O[i]);
                    end;
            end;
        end;

    end;

    {
        Removes entries from check which are equal to compareTo
    }
    procedure removeEqualObjectEntries(check, compareTo: TJsonObject);
    var
        i, curType: integer;
        curName: string;
        namesToRemove: TStringList;
    begin
        namesToRemove := TStringList.create;
        for i:=0 to check.count-1 do begin
            curName := check.Names[i];
            curType := check.Types[curName];
            case curType of
                JSON_TYPE_ARRAY:
                    begin
                        if(check.A[curName].toString() = compareTo.A[curName].toString()) then begin
                            namesToRemove.add(curName);
                        end else begin
                            removeEqualArrayEntries(check.A[curName], compareTo.A[curName]);
                            if(check.A[curName].count = 0) then begin
                                namesToRemove.add(curName);
                            end;
                        end;
                    end;
                JSON_TYPE_OBJECT:
                    begin
                        removeEqualObjectEntries(check.O[curName], compareTo.O[curName]);
                        if(check.O[curName].count = 0) then begin
                            namesToRemove.add(curName);
                        end;
                    end;
                else
                    begin
                        if(check.S[curName] = compareTo.S[curName]) then begin
                            // now what?
                            namesToRemove.add(curName);
                        end;
                    end;
            end;
        end;

        for i:=0 to namesToRemove.count-1 do begin
            check.remove(namesToRemove[i]);
        end;
        namesToRemove.free();
    end;

    function getObjectByPath(src: TJsonObject; objPath: string): TJsonObject;
    var
        tmpList: TStringList;
        curSubpath: string;
        i: integer;
    begin
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        Result := src;

        for i:=0 to tmpList.count-1 do begin
            curSubpath := tmpList[i];
            Result := Result.O[curSubpath];
        end;

        tmpList.free();
    end;

    function getTypeAtPath(src: TJsonObject; objPath: string): int;
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin
        Result := JSON_TYPE_NONE;
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := tmpList[i];
            if(i < lastIndex) then begin
                if(pathObj.Types[curSubpath] <> JSON_TYPE_OBJECT) then begin
                    tmpList.free();
                    exit;
                end;
                pathObj := pathObj.O[curSubpath];
            end else begin
                Result := pathObj.Types[curSubpath];
            end;
        end;

        tmpList.free();
    end;

    function getVariantValueAtPath(src: TJsonObject; objPath: string; default: variant): string;
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin
        Result := default;
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := tmpList[i];
            if(pathObj.Types[curSubpath] = JSON_TYPE_NONE) then begin
                tmpList.free();
                exit;
            end;

            if(i < lastIndex) then begin
                pathObj := pathObj.O[curSubpath];
            end else begin
                Result := getJsonObjectValueVariant(pathObj, curSubpath, default);
            end;
        end;

        tmpList.free();
    end;


    function getStringValueAtPath(src: TJsonObject; objPath: string; default: string): string;
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin
        Result := default;
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := tmpList[i];
            if(pathObj.Types[curSubpath] = JSON_TYPE_NONE) then begin
                tmpList.free();
                exit;
            end;

            if(i < lastIndex) then begin
                pathObj := pathObj.O[curSubpath];
            end else begin
                Result := pathObj.S[curSubpath];
            end;
        end;

        tmpList.free();
    end;

    procedure putStringValueAtPath(src: TJsonObject; objPath: string; value: string);
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := tmpList[i];
            if(i < lastIndex) then begin
                pathObj := pathObj.O[curSubpath];
            end else begin
                pathObj.S[curSubpath] := value;
            end;
        end;

        tmpList.free();
    end;

    function getArrayValueAtPath(src: TJsonObject; objPath: string): string;
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin
        Result := default;
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := tmpList[i];
            if(i < lastIndex) then begin
                pathObj := pathObj.O[curSubpath];
            end else begin
                Result := pathObj.A[curSubpath];
            end;
        end;

        tmpList.free();
    end;

    function putArrayValueAtPath(src: TJsonObject; objPath: string; value: TJsonArray): string;
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := tmpList[i];
            if(i < lastIndex) then begin
                pathObj := pathObj.O[curSubpath];
            end else begin
                pathObj.A[curSubpath] := cloneJsonArray(value);
            end;
        end;

        tmpList.free();
    end;

    function getObjectValueAtPath(src: TJsonObject; objPath: string): string;
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin
        Result := default;
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := tmpList[i];
            if(i < lastIndex) then begin
                pathObj := pathObj.O[curSubpath];
            end else begin
                Result := pathObj.O[curSubpath];
            end;
        end;

        tmpList.free();
    end;

    function putObjectValueAtPath(src: TJsonObject; objPath: string; value: TJsonObject): string;
    var
        tmpList: TStringList;
        curSubpath: string;
        i, lastIndex: integer;
        pathObj: TJsonObject;
    begin
        tmpList := TStringList.create;
        tmpList.Delimiter := '\';
        tmpList.StrictDelimiter := true;
        tmpList.DelimitedText := objPath;
        pathObj := src;

        lastIndex := tmpList.count-1;
        for i:=0 to lastIndex do begin
            curSubpath := tmpList[i];
            if(i < lastIndex) then begin
                pathObj := pathObj.O[curSubpath];
            end else begin
                pathObj.O[curSubpath] := cloneJsonObject(value);
            end;
        end;

        tmpList.free();
    end;

    procedure setJsonObjectValueVariant(targetJson: TJsonObject; key: string; v: variant; fallbackV: string);
    var
        variantType: integer;
    begin
        variantType := (varType(v) and VarTypeMask);

        case variantType of
            varInteger, 18:
                targetJson.I[key] := v;
            19, 20:
                targetJson.L[key] := v;
            varDouble:
                targetJson.F[key] := v;
            varString, varUString:
                targetJson.S[key] := v;
            varBoolean:
                targetJson.B[key] := v;
            else
                targetJson.S[key] := fallbackV;
        end;
    end;

    function getJsonObjectValueVariant(obj: TJsonObject; key: string; default: variant): variant;
    var
        curType: integer;
    begin
        Result := default;
        curType := obj.Types[key];
        case curType of
            JSON_TYPE_STRING:
                Result := obj.S[key];
            JSON_TYPE_INT:
                Result := obj.I[key];
            JSON_TYPE_LONG:
                Result := obj.L[key];
            JSON_TYPE_ULONG:
                Result := obj.U[key];
            JSON_TYPE_FLOAT:
                Result := obj.F[key];
            JSON_TYPE_DATETIME:
                Result := obj.D[key];
            JSON_TYPE_BOOL:
                Result := obj.B[key];
            JSON_TYPE_ARRAY:
                Result := obj.A[key];
            JSON_TYPE_OBJECT:
                Result := obj.O[key];
        end;
    end;

    function getJsonArrayValueVariant(arr: TJsonArray; key: integer; default: variant): variant;
    var
        curType: integer;
    begin
        Result := default;
        curType := arr.Types[key];
        case curType of
            JSON_TYPE_STRING:
                Result := arr.S[key];
            JSON_TYPE_INT:
                Result := arr.I[key];
            JSON_TYPE_LONG:
                Result := arr.L[key];
            JSON_TYPE_ULONG:
                Result := arr.U[key];
            JSON_TYPE_FLOAT:
                Result := arr.F[key];
            JSON_TYPE_DATETIME:
                Result := arr.D[key];
            JSON_TYPE_BOOL:
                Result := arr.B[key];
            JSON_TYPE_ARRAY:
                Result := arr.A[key];
            JSON_TYPE_OBJECT:
                Result := arr.O[key];
        end;
    end;

    {
        "typesafe" getJsonObjectValue, returns value if the given type matches, default otherwise
    }
    function getJsonObjectValueTS(obj: TJsonObject; key: string; valType: integer; default: variant): variant;
    begin
        Result := default;
        if(obj.Types[key] <> valType) then exit;

        Result := getJsonObjectValueVariant(obj, key, default);
    end;

    {
        getJsonObjectValue with typecasting, tries to cast the value to the given type.
        Doesn't support casting to date, array, or object.
    }
    function getJsonObjectValueTC(obj: TJsonObject; key: string; targetType: integer; default: variant): variant;
    var
        valType: integer;
    begin
        valType := obj.Types[key];
        Result := default;
        case valType of
            JSON_TYPE_STRING:
                begin
                    case targetType of
                        JSON_TYPE_STRING:   Result := obj.S[key];
                        JSON_TYPE_INT:      Result := StrToInt(obj.S[key]);
                        JSON_TYPE_LONG:     Result := StrToInt(obj.S[key]);
                        JSON_TYPE_ULONG:    Result := StrToInt(obj.S[key]);
                        JSON_TYPE_FLOAT:    Result := StrToFloat(obj.S[key]);
                        // JSON_TYPE_DATETIME  = 6; // datetime I honestly don't know
                        JSON_TYPE_BOOL:     Result := StrToBool(obj.S[key]);
                        //JSON_TYPE_ARRAY     Result := // mabye add parsing?
                        //JSON_TYPE_OBJECT    Result :=
                    end;
                end;
            JSON_TYPE_INT:
                begin
                    case targetType of
                        JSON_TYPE_STRING:   Result := IntToStr(obj.I[key]);
                        JSON_TYPE_INT:      Result := obj.I[key];
                        JSON_TYPE_LONG:     Result := obj.I[key];
                        JSON_TYPE_ULONG:    Result := abs(obj.I[key]);
                        JSON_TYPE_FLOAT:    Result := (obj.I[key] * 1.0);
                        JSON_TYPE_BOOL:     Result := (obj.I[key] <> 0);
                    end;
                end;
            JSON_TYPE_LONG:
                begin
                    case targetType of
                        JSON_TYPE_STRING:   Result := IntToStr(obj.L[key]);
                        JSON_TYPE_INT:      Result := obj.L[key];
                        JSON_TYPE_LONG:     Result := obj.L[key];
                        JSON_TYPE_ULONG:    Result := abs(obj.L[key]);
                        JSON_TYPE_FLOAT:    Result := (obj.L[key] * 1.0);
                        JSON_TYPE_BOOL:     Result := (obj.L[key] <> 0);
                    end;
                end;
            JSON_TYPE_ULONG:
                begin
                    case targetType of
                        JSON_TYPE_STRING:   Result := IntToStr(obj.U[key]);
                        JSON_TYPE_INT:      Result := obj.U[key];
                        JSON_TYPE_LONG:     Result := obj.U[key];
                        JSON_TYPE_ULONG:    Result := obj.U[key];
                        JSON_TYPE_FLOAT:    Result := (obj.U[key] * 1.0);
                        JSON_TYPE_BOOL:     Result := (obj.U[key] <> 0);
                    end;
                end;
            JSON_TYPE_FLOAT:
                begin
                    case targetType of
                        JSON_TYPE_STRING:   Result := FloatToStr(obj.L[key]);
                        JSON_TYPE_INT:      Result := Trunc(obj.F[key]);
                        JSON_TYPE_LONG:     Result := Trunc(obj.F[key]);
                        JSON_TYPE_ULONG:    Result := abs(Trunc(obj.F[key]));
                        JSON_TYPE_FLOAT:    Result := obj.F[key];
                        JSON_TYPE_BOOL:     Result := (obj.F[key] <> 0);
                    end;
                end;
            JSON_TYPE_BOOL:
                begin
                    case targetType of
                        JSON_TYPE_STRING:   Result := BoolToStr(obj.B[key]);
                        JSON_TYPE_INT:      Result := ternaryOp(obj.B[key], 1, 0);
                        JSON_TYPE_LONG:     Result := ternaryOp(obj.B[key], 1, 0);
                        JSON_TYPE_ULONG:    Result := ternaryOp(obj.B[key], 1, 0);
                        JSON_TYPE_FLOAT:    Result := ternaryOp(obj.B[key], 1.0, 0.0);
                        JSON_TYPE_BOOL:     Result := obj.B[key];
                    end;
                end;

        end;
    end;

    function indexOfTJsonArrayS(jsonArray: TJsonArray; value: string): integer;
    var
        i: integer;
        valLc: string;
    begin
        valLc := LowerCase(value);
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            if(jsonArray.Types[i] = JSON_TYPE_STRING) then begin
                if(LowerCase(jsonArray.S[i]) = valLc) then begin
                    Result := i;
                    exit;
                end;
            end;
        end;
    end;

    function indexOfTJsonArraySubstr(jsonArray: TJsonArray; value: string): integer;
    var
        i: integer;
        valLc, curStr: string;
    begin
        valLc := LowerCase(value);
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            curStr := LowerCase(jsonArray.S[i]);

            if (pos(valLc, curStr) > 0) then begin
                Result := i;
                exit;
            end;
        end;
    end;

    function indexOfTJsonArrayPrefix(jsonArray: TJsonArray; value: string): integer;
    var
        i: integer;
        valLc, curStr: string;
    begin
        valLc := LowerCase(value);
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            curStr := LowerCase(jsonArray.S[i]);

            if (strStartsWith(curStr, valLc)) then begin
                Result := i;
                exit;
            end;
        end;
    end;

    function indexOfTJsonArraySuffix(jsonArray: TJsonArray; value: string): integer;
    var
        i: integer;
        valLc, curStr: string;
    begin
        valLc := LowerCase(value);
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            curStr := LowerCase(jsonArray.S[i]);

            if (strEndsWith(curStr, valLc)) then begin
                Result := i;
                exit;
            end;
        end;
    end;

    function indexOfTJsonArrayI(jsonArray: TJsonArray; value: integer): integer;
    var
        i: integer;
    begin
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            if(jsonArray.Types[i] = JSON_TYPE_INT) then begin
                if(jsonArray.I[i] = value) then begin
                    Result := i;
                    exit;
                end;
            end;
        end;
    end;

    function indexOfTJsonArrayL(jsonArray: TJsonArray; value: integer): integer;
    var
        i: integer;
    begin
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            if(jsonArray.Types[i] = JSON_TYPE_LONG) then begin
                if(jsonArray.L[i] = value) then begin
                    Result := i;
                    exit;
                end;
            end;
        end;
    end;

    function indexOfTJsonArrayU(jsonArray: TJsonArray; value: cardinal): integer;
    var
        i: integer;
    begin
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            if(jsonArray.Types[i] = JSON_TYPE_ULONG) then begin
                if(jsonArray.U[i] = value) then begin
                    Result := i;
                    exit;
                end;
            end;
        end;
    end;

    function indexOfTJsonArrayF(jsonArray: TJsonArray; value: float): integer;
    var
        i: integer;
    begin
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            if(jsonArray.Types[i] = JSON_TYPE_FLOAT) then begin
                if(jsonArray.F[i] = value) then begin
                    Result := i;
                    exit;
                end;
            end;
        end;
    end;

    function indexOfTJsonArrayD(jsonArray: TJsonArray; value: TDateTime): integer;
    var
        i: integer;
    begin
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            if(jsonArray.Types[i] = JSON_TYPE_DATETIME) then begin
                if(jsonArray.D[i] = value) then begin
                    Result := i;
                    exit;
                end;
            end;
        end;
    end;

    function indexOfTJsonArrayB(jsonArray: TJsonArray; value: boolean): integer;
    var
        i: integer;
    begin
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            if(jsonArray.Types[i] = JSON_TYPE_BOOL) then begin
                if(jsonArray.B[i] = value) then begin
                    Result := i;
                    exit;
                end;
            end;
        end;
    end;

    function indexOfTJsonArrayA(jsonArray: TJsonArray; value: TJsonArray): integer;
    var
        i: integer;
        cmpVal: string;
    begin
        cmpVal := value.toString();
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            if(jsonArray.Types[i] = JSON_TYPE_ARRAY) then begin
                if(jsonArray.A[i].toString() = cmpVal) then begin
                    Result := i;
                    exit;
                end;
            end;
        end;
    end;

    function indexOfTJsonArrayO(jsonArray: TJsonArray; value: TJsonObject): integer;
    var
        i: integer;
        cmpVal: string;
    begin
        cmpVal := value.toString();
        Result := -1;
        for i:=0 to jsonArray.count-1 do begin
            if(jsonArray.Types[i] = JSON_TYPE_OBJECT) then begin
                if(jsonArray.O[i].toString() = cmpVal) then begin
                    Result := i;
                    exit;
                end;
            end;
        end;
    end;

    procedure mergeJsonArrayUnique(jsonTarget, jsonSource: TJsonArray);
    var
        i, curType: integer;
        newArray: TJsonArray;
        newObject: TJsonObject;
    begin

        for i:=0 to jsonSource.count-1 do begin
            curType := jsonSource.Types[i];
            case curType of
                JSON_TYPE_STRING:  // string
                    begin
                        if(indexOfTJsonArrayS(jsonTarget, jsonSource.S[i]) < 0) then begin
                            jsonTarget.add(jsonSource.S[i]);
                        end;
                    end;
                JSON_TYPE_INT: // int
                    begin
                        if(indexOfTJsonArrayI(jsonTarget, jsonSource.I[i]) < 0) then begin
                            jsonTarget.add(jsonSource.I[i]);
                        end;
                    end;
                JSON_TYPE_LONG: // long
                    begin
                        if(indexOfTJsonArrayL(jsonTarget, jsonSource.L[i]) < 0) then begin
                            jsonTarget.add(jsonSource.L[i]);
                        end;
                    end;
                JSON_TYPE_ULONG: // ulong
                    begin
                        if(indexOfTJsonArrayU(jsonTarget, jsonSource.U[i]) < 0) then begin
                            jsonTarget.add(jsonSource.U[i]);
                        end;
                    end;
                JSON_TYPE_FLOAT: // float
                    begin
                        if(indexOfTJsonArrayF(jsonTarget, jsonSource.F[i]) < 0) then begin
                            jsonTarget.add(jsonSource.F[i]);
                        end;
                    end;
                JSON_TYPE_DATETIME: // datetime
                    begin
                        if(indexOfTJsonArrayD(jsonTarget, jsonSource.D[i]) < 0) then begin
                            jsonTarget.add(jsonSource.D[i]);
                        end;
                    end;
                JSON_TYPE_BOOL: // bool
                    begin
                        if(indexOfTJsonArrayB(jsonTarget, jsonSource.B[i]) < 0) then begin
                            jsonTarget.add(jsonSource.B[i]);
                        end;
                    end;
                JSON_TYPE_ARRAY: // array
                    begin
                        if(indexOfTJsonArrayA(jsonTarget, jsonSource.A[i]) < 0) then begin
                            // append source[i] as new array
                            newArray := jsonTarget.addArray();
                            mergeJsonArray(newArray, jsonSource.A[i]);
                        end;
                    end;
                JSON_TYPE_OBJECT: // object
                    begin
                        if(indexOfTJsonArrayO(jsonTarget, jsonSource.O[i]) < 0) then begin
                            // append source[i] as new object
                            newObject := jsonTarget.addObject();
                            mergeJsonObject(newObject, jsonSource.O[i]);
                        end;
                    end;
            end;
        end;
    end;

    procedure mergeJsonArray(jsonTarget, jsonSource: TJsonArray);
    var
        i, curType: integer;
        newArray: TJsonArray;
        newObject: TJsonObject;
    begin
        for i:=0 to jsonSource.count-1 do begin
            curType := jsonSource.Types[i];
            addValueToArray(jsonTarget, getJsonArrayValueVariant(jsonSource, i, ''), curType);
        end;
    end;

    function cloneJsonObject(orig: TJsonObject): TJsonObject;
    begin
        Result := TJsonObject.create();
        Result.assign(orig);
    end;

    function cloneJsonArray(orig: TJsonArray): TJsonArray;
    begin
        Result := TJsonArray.create();
        Result.assign(orig);
    end;

    procedure prependConcatJsonArrays(jsonTarget, jsonSource: TJsonArray);
    var
        arrayBak: TJsonArray;
    begin
        arrayBak := cloneJsonArray(jsonTarget);
        jsonTarget.clear();

        concatJsonArrays(jsonTarget, jsonSource);
        concatJsonArrays(jsonTarget, arrayBak);

        arrayBak.free();
    end;

    procedure concatJsonArrays(jsonTarget, jsonSource: TJsonArray);
    var
        i, curType: integer;
        newArray: TJsonArray;
        newObject: TJsonObject;
    begin
        for i:=0 to jsonSource.count-1 do begin
            curType := jsonSource.Types[i];
            case curType of
                JSON_TYPE_STRING:   jsonTarget.add(jsonSource.S[i]);
                JSON_TYPE_INT:      jsonTarget.add(jsonSource.I[i]);
                JSON_TYPE_LONG:     jsonTarget.add(jsonSource.L[i]);
                JSON_TYPE_ULONG:    jsonTarget.add(jsonSource.U[i]);
                JSON_TYPE_FLOAT:    jsonTarget.add(jsonSource.F[i]);
                JSON_TYPE_DATETIME: jsonTarget.add(jsonSource.D[i]);
                JSON_TYPE_BOOL:     jsonTarget.add(jsonSource.B[i]);
                JSON_TYPE_ARRAY: // array
                    begin
                        // append source[i] as new array
                        newArray := jsonTarget.addArray();
                        mergeJsonArray(newArray, jsonSource.A[i]);
                    end;
                JSON_TYPE_OBJECT: // object
                    begin
                        // append source[i] as new object
                        newObject := jsonTarget.addObject();
                        mergeJsonObject(newObject, jsonSource.O[i]);
                    end;
            end;

        end;
    end;

    procedure mergeJsonObject(jsonTarget, jsonSource: TJsonObject);
    var
        i, curType: integer;
        key: string;
    begin
        for i:=0 to jsonSource.count-1 do begin
            key := jsonSource.names[i];
            curType := jsonSource.Types[key];

            case curType of
                JSON_TYPE_STRING:  // string
                    begin
                        jsonTarget.S[key] := jsonSource.S[key];
                    end;
                JSON_TYPE_INT: // int
                    begin
                        jsonTarget.I[key] := jsonSource.I[key];
                    end;
                JSON_TYPE_LONG: // long
                    begin
                        jsonTarget.L[key] := jsonSource.L[key];
                    end;
                JSON_TYPE_ULONG: // ulong
                    begin
                        jsonTarget.U[key] := jsonSource.U[key];
                    end;
                JSON_TYPE_FLOAT: // float
                    begin
                        jsonTarget.F[key] := jsonSource.F[key];
                    end;
                JSON_TYPE_DATETIME: // datetime
                    begin
                        jsonTarget.D[key] := jsonSource.D[key];
                    end;
                JSON_TYPE_BOOL: // bool
                    begin
                        jsonTarget.B[key] := jsonSource.B[key];
                    end;
                JSON_TYPE_ARRAY: // array
                    begin
                        mergeJsonArray(jsonTarget.A[key], jsonSource.A[key]);
                    end;
                JSON_TYPE_OBJECT: // object
                    begin
                        mergeJsonObject(jsonTarget.O[key], jsonSource.O[key]);
                    end;
            end;
        end;
    end;

    procedure prependStringToArray(arr: TJsonArray; s: string);
    var
        arrBackup: TJsonArray;
        newObj: TJsonObject;
    begin
        arrBackup := cloneJsonArray(arr);
        arr.clear();

        arr.add(s);

        concatJsonArrays(arr, arrBackup);
        arrBackup.free();
    end;

    procedure prependObjectToArray(arr: TJsonArray; obj: TJsonObject);
    var
        arrBackup: TJsonArray;
        newObj: TJsonObject;
    begin
        arrBackup := cloneJsonArray(arr);
        arr.clear();

        newObj := arr.addObject();
        mergeJsonObject(newObj, obj);

        concatJsonArrays(arr, arrBackup);
        arrBackup.free();
    end;

    procedure appendObjectToArray(arr: TJsonArray; obj: TJsonObject);
    var
        newObj : TJsonObject;
    begin
        newObj := arr.addObject();
        mergeJsonObject(newObj, obj);
    end;

    procedure insertObjectIntoArray(arr: TJsonArray; obj: TJsonObject; index: integer);
    var
        i: integer;
        curObj, nextObj: TJsonObject;
    begin
        if(index = arr.count) then begin
            appendObjectToArray(arr, obj);
            exit;
        end;

        if(index = 0) then begin
            prependObjectToArray(arr, obj);
            exit;
        end;

        if(index > arr.count) then begin
            for i:=0 to index do begin
                curObj := arr.addObject();
            end;
            mergeJsonObject(curObj, obj);
            exit;
        end;

        // move everything by one forward
        arr.addObject();
        for i:=arr.count-1 downto index+1 do begin
            nextObj := arr.O[i];
            curObj := arr.O[i-1];

            mergeJsonObject(nextObj, curObj);
            curObj.clear();
        end;

        curObj := arr.O[index];
        mergeJsonObject(curObj, obj);
    end;

    procedure appendArrayToArray(arr1: TJsonArray; arr2: TJsonArray);
    var
        newArr : TJsonArray;
    begin
        newArr := arr1.addArray();
        mergeJsonArray(newArr, arr2);
    end;

    procedure prependArrayToArray(arr: TJsonArray; obj: TJsonObject);
    var
        arrBackup: TJsonArray;
        newArr: TJsonObject;
    begin
        arrBackup := cloneJsonArray(arr);
        arr.clear();

        newArr := arr.addArray();
        mergeJsonArray(newArr, obj);

        concatJsonArrays(arr, arrBackup);
        arrBackup.free();
    end;

    {
        Removes the item from the array at the given index, and returns it
    }
    function removeFromArray(arr: TJsonArray; key: integer): variant;
    var
        curType: integer;
    begin
        curType := arr.Types[key];
        case curType of
            JSON_TYPE_STRING:
                Result := arr.S[key];
            JSON_TYPE_INT:
                Result := arr.I[key];
            JSON_TYPE_LONG:
                Result := arr.L[key];
            JSON_TYPE_ULONG:
                Result := arr.U[key];
            JSON_TYPE_FLOAT:
                Result := arr.F[key];
            JSON_TYPE_DATETIME:
                Result := arr.D[key];
            JSON_TYPE_BOOL:
                Result := arr.B[key];
            JSON_TYPE_ARRAY:
                Result := cloneJsonArray(arr.A[key]);
            JSON_TYPE_OBJECT:
                Result := cloneJsonObject(arr.O[key]);
        end;

        arr.delete(key);
    end;

    procedure appendValueToArray(arr: TJsonArray; value: variant; valueType: integer);
    begin
        case valueType of
            JSON_TYPE_ARRAY:    appendArrayToArray(arr, value);
            JSON_TYPE_OBJECT:   appendObjectToArray(arr, value);
            else                arr.add(value);
        end;
    end;

    procedure prependValueToArray(arr: TJsonArray; value: variant; valueType: integer);
    var
        arrBackup: TJsonArray;
    begin
        case valueType of
            JSON_TYPE_ARRAY:    prependArrayToArray(arr, value);
            JSON_TYPE_OBJECT:   prependObjectToArray(arr, value);
            else
                begin
                    arrBackup := cloneJsonArray(arr);
                    arr.clear();

                    arr.add(value);

                    concatJsonArrays(arr, arrBackup);
                    arrBackup.free();
                end;
        end;
    end;

    procedure addValueToArray(jsonTarget: TJsonArray; value: variant; valueType: integer);
    var
        newArray: TJsonArray;
        newObject: TJsonObject;
    begin

        if(indexOfTJsonArrayVariant(jsonTarget, value, valueType) < 0) then begin
            appendValueToArray(jsonTarget, value, valueType);
        end;
    end;

    procedure setArrayValue(target: TJsonArray; index: integer; value: variant; valueType: integer);
    begin
        case valueType of
            JSON_TYPE_STRING:  // string
                target.S[index] := value;
            JSON_TYPE_INT: // int
                target.I[index] := value;
            JSON_TYPE_LONG: // long
                target.L[index] := value;
            JSON_TYPE_ULONG: // ulong
                target.U[index] := value;
            JSON_TYPE_FLOAT: // float
                target.F[index] := value;
            JSON_TYPE_DATETIME: // datetime
                target.D[index] := value;
            JSON_TYPE_BOOL: // bool
                target.B[index] := value;
            JSON_TYPE_ARRAY: // array
                target.A[index] := cloneJsonArray(value);
            JSON_TYPE_OBJECT: // object
                target.O[index] := cloneJsonObject(value);
        end;
    end;

    procedure setObjectValue(target: TJsonObject; key: string; value: variant; valueType: integer);
    begin
        case valueType of
            JSON_TYPE_STRING:  // string
                target.S[key] := value;
            JSON_TYPE_INT: // int
                target.I[key] := value;
            JSON_TYPE_LONG: // long
                target.L[key] := value;
            JSON_TYPE_ULONG: // ulong
                target.U[key] := value;
            JSON_TYPE_FLOAT: // float
                target.F[key] := value;
            JSON_TYPE_DATETIME: // datetime
                target.D[key] := value;
            JSON_TYPE_BOOL: // bool
                target.B[key] := value;
            JSON_TYPE_ARRAY: // array
                target.A[key] := cloneJsonArray(value);
            JSON_TYPE_OBJECT: // object
                target.O[key] := cloneJsonObject(value);
        end;
    end;

    function indexOfTJsonArrayVariant(jsonTarget: TJsonArray; value: variant; valueType: integer): integer;
    var
        newArray: TJsonArray;
        newObject: TJsonObject;
    begin
        Result := -1;
        case valueType of
            JSON_TYPE_STRING:  // string
                Result := indexOfTJsonArrayS(jsonTarget, value);
            JSON_TYPE_INT: // int
                Result := indexOfTJsonArrayI(jsonTarget, value);
            JSON_TYPE_LONG: // long
                Result := indexOfTJsonArrayL(jsonTarget, value);
            JSON_TYPE_ULONG: // ulong
                Result := indexOfTJsonArrayU(jsonTarget, value);
            JSON_TYPE_FLOAT: // float
                Result := indexOfTJsonArrayF(jsonTarget, value);
            JSON_TYPE_DATETIME: // datetime
                Result := indexOfTJsonArrayD(jsonTarget, value);
            JSON_TYPE_BOOL: // bool
                Result := indexOfTJsonArrayB(jsonTarget, value);
            JSON_TYPE_ARRAY: // array
                Result := indexOfTJsonArrayA(jsonTarget, value);
            JSON_TYPE_OBJECT: // object
                Result := indexOfTJsonArrayO(jsonTarget, value);
        end;
    end;

    // === debug functions ===
    {
        Produces a formatted output of the given element, prepends the prefix to each line
    }
    procedure dumpElemWithPrefix(e: IInterface; prefix: String);
    var
        i: Integer;
        child: IInterface;
    begin
        for i := 0 to ElementCount(e)-1 do begin
            child := ElementByIndex(e, i);
            AddMessage(prefix+DisplayName(child)+'='+GetEditValue(child));
            dumpElemWithPrefix(child, prefix+'  ');
        end;
    end;

    {
        Produces a formatted output of the given element
    }
    procedure dumpElem(e: IInterface);
    begin
        dumpElemWithPrefix(e, '');
    end;
end.
