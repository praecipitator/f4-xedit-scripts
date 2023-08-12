{
    Poor man's PEX "decompiler".
    Doesn't actually fully decompile it, but provides some info about the pex

    Sources:
        - http://f4se.silverlock.org/
            The source code of scriptdump
        - https://en.uesp.net/wiki/Tes5Mod:Compiled_Script_File_Format
            (Kinda, that is for Skyrim, F4 is similar, but different)

    Usage:
        - call either readPexResource, pexReadFile, or pexReadStream. It will return a TJsonObject. Clean it up manually.

    Output Json structure:
        - "header": object
            - "majorVersion": int
            - "minorVersion": int
            - "gameId": int
            - "compileTime1": uint
            - "compileTime2": uint
            - "sourceName": string
            - "userName": string
            - "machineName": string
        - "userFlags": object
            Contains this:
                "0": "hidden",
                "1": "conditional",
                "2": "default",
                "3": "collapsedonref",
                "4": "collapsedonbase",
                "5": "mandatory"
            Essentially the same as th PEX_FLAG_ constants.
            The 0-5 numbers are the leftshift argument, that is, "5" for "mandatory" means it's actually 1 shl 5 = 32
            These are also represented by the PEX_FLAG_* constants
        - "objects": array of PEXObject
            The objects represent the PEXObjects ("classes") contained within the pex file. Usually only one,
            but it seems like the format supports multiple, too.

    PEXObject structure:
        - "name": string
        - "extends": string
        - "docblock": string
        - "const": bool
        - "userFlags": int
        - "autoStateName": string
            if no auto state is defined, this will be ""
        - "structs": array of PEXStruct
        - "variables": array of PEXVariable
        - "properties": array of PEXProperty
        - "states": array of PEXState
        
    PEXStruct structure:
        - "name": string
        - "members": array of PEXStructMember
        
    PEXStructMember structure:
        - "name": string
        - "type": string
        - "userFlags": int
        - "value": PEXValue
            this represents the default value of the variable
        - "const": boolean
        - "docblock": string

    PEXVariable structure:
        - "name": string
            if this begins with "::" and ends with "_var", then it's an auto-generated local variable of a property.
        - "type": string
        - "userFlags": int
        - "value": PEXValue
            this represents the default value of the variable
        - "const": bool

    PEXProperty structure:
        - "name": string
        - "type": string
        - "docblock": string
        - "userFlags": int
        - "flags": int

    PEXState structure:
        - "name": string
            This is "" for the "empty state"
        - "functions": array of PEXFunction

    PEXFunction structure:
        - "name": string
        - "data": object
            - "returnType": string
            - "docBlock": string
            - "userFlags": int
            - "flags": int
            - "params": PEXParam
            - "locals": PEXParam
        This doesn't contain any actual code of the function
        
    PEXParam structure:
        - "name": string
        - "type": string

}
unit PexToJson;
    const
        PEX_FLAG_HIDDEN = 1;
        PEX_FLAG_CONDITIONAL = 2;
        PEX_FLAG_DEFAULT = 4;
        PEX_FLAG_COLLAPSEDONREF = 8;
        PEX_FLAG_COLLAPSEDONBASE = 16;
        PEX_FLAG_MANDATORY = 32;

    var
        pexStringTable: TStringList;
        pexBr: TBinaryReader;
        pexCurrentStream: TStream;

    function readPexScriptName(scriptName: string): TJsonObject;
	begin
		Result := readPexResource(_scriptNameToPexPath(scriptName));
	end;

	function readPexResource(filename: string): TJsonObject;
	var
		containers, assets: TStringList;
		i: integer;
		foundContainer: string;
		byteStream: TFileStream;
	begin
		Result := nil;
		// this is the shitty part...
		// So, there is ResourceExists. It checks whenever the thing exists in any container, but doesn't tell you in which.
		// There is also ResourceOpenData. This one REQUIRES the container.
		// So, I guess I have to go find the container manually...

		containers := TStringList.create();
		ResourceContainerList(containers);

		for i:=containers.count-1 downto 0 do begin
			assets := TStringList.create();

			containers.CaseSensitive := false;
			ResourceList(containers[i], assets);

			if(assets.indexOf(filename) >= 0) then begin
				foundContainer := containers[i];
				assets.free();
				break;
			end;
			assets.free();
		end;
		containers.free();

		if(foundContainer = '') then exit;

		// ok try loading
		byteStream := TBytesStream.Create(ResourceOpenData(foundContainer, filename));
		Result := pexReadStream(byteStream);
		byteStream.free();
	end;

    {
        filename MUST be within DataPath!
    }
    function pexReadFile(filename: string): TJsonObject;
    var
        byteStream: TFileStream;
    begin
        Result := nil;


        if(not FileExists(DataPath+filename)) then begin
            exit;
        end;

        //pexContainedObjects := TStringList.create;
        //pexExtendedObjects  := TStringList.create;

        try
            byteStream := TBytesStream.Create(ResourceOpenData('', filename));

            Result := pexReadStream(byteStream);
        finally
            if(assigned(byteStream)) then begin
                byteStream.free();
            end;
        end;

    end;

    {
        byteStream can be made by either
            TBytesStream.Create(ResourceOpenData(...))
        or
            TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite)
    }
    function pexReadStream(byteStream: TFileStream): TJsonObject;
    begin
        Result := nil;

        //pexContainedObjects := TStringList.create;
        //pexExtendedObjects  := TStringList.create;

        pexCurrentStream := byteStream;

        pexBr := TBinaryReader.Create(byteStream);

        Result := _pexDoParse();

        _pexCleanUp();
    end;

    // ==========================================================================
    // === END of "public" functions. Please do not call anything below manually.
    // ==========================================================================

    // TODO: also port checkScriptExtends and findScriptInElementByName

    procedure _pexCleanUp();
    begin
        if(assigned(pexStringTable)) then begin
            pexStringTable.free();
            pexStringTable := nil;
        end;

        if(assigned(pexBr)) then begin
            pexBr.free();
            pexBr := nil;
        end;
    end;

    function _pexGetStringTableEntry(i: integer): string;
    begin
        if(i<pexStringTable.count) then begin
            Result := pexStringTable[i];
            exit;
        end;

        Result := '';
    end;

    function _pexReadHeader(): TJsonObject;
    var
        magic: cardinal;
        pexMajorVersion, pexMinorVersion, pexGameId: integer;
        pexCompileTime1: cardinal;
        pexCompileTime2: cardinal;
        pexSourceName, pexUserName, pexMachineName: string;
    begin
        Result := TJsonObject.create();
        magic := pexBr.readUInt32(); // magic
 //       Result.U['magic'] := magic;

        //pexBr.seek(4);//seek(4); // soBegin, soCurrent, soEnd
        pexMajorVersion := pexBr.readByte(); // just read = 1 byte
        pexMinorVersion := pexBr.readByte();
        pexGameId := pexBr.ReadUInt16();

        Result.I['majorVersion'] := pexMajorVersion;
        Result.I['minorVersion'] := pexMinorVersion;
        Result.I['gameId'] := pexGameId;

        // it seems pascal doesn't support 64bit ints
        pexCompileTime1 := pexBr.ReadUInt32();
        pexCompileTime2 := pexBr.ReadUInt32();

        Result.U['compileTime1'] := pexCompileTime1;
        Result.U['compileTime2'] := pexCompileTime2;

        pexSourceName  := _pexReadString();
        pexUserName    := _pexReadString();
        pexMachineName := _pexReadString();

        Result.S['sourceName']  := pexSourceName;
        Result.S['userName']    := pexUserName;
        Result.S['machineName'] := pexMachineName;


    end;

    procedure _pexReadStringTable();
    var
        numStrings, i: integer;
        curString: string;
    begin
        pexStringTable := TStringList.create;

        numStrings := pexBr.ReadUInt16();
        for i:=0 to numStrings-1 do begin
            curString := _pexReadString();
            //AddMessage('Found string '+curString);
            pexStringTable.add(curString);
        end;
    end;

    procedure _pexReadDebugFunction();
    var
        objectNameIndex, stateNameIndex, functionNameIndex, functionType, instructionCount, i: integer;
    begin
        objectNameIndex := pexBr.ReadUInt16();
        stateNameIndex  := pexBr.ReadUInt16();
        functionNameIndex := pexBr.ReadUInt16();
        functionType := pexBr.readByte();
        instructionCount := pexBr.ReadUInt16();

        skipBytes(instructionCount * 2);
        {
        AddMessage('objectNameIndex '+pexStringTable[objectNameIndex]+', stateNameIndex '+pexStringTable[stateNameIndex]+', functionNameIndex '+pexStringTable[functionNameIndex]);
        AddMessage('NumInstr '+IntToStr(instructionCount));
        for i:=0 to instructionCount-1 do begin
            // potentially do something with the info? or just skip 2*instructionCount right away?
            AddMessage('    Line NR: '+IntToStr(pexBr.ReadUInt16()));
        end;
        }
    end;

    procedure _pexReadPropertyGroups();
    var
        cnt, objName, groupName, groupDoc, nameCount, i, j: integer;
        userFlags: cardinal;
        curGroupName: string;
    begin
        // Seems to be Groups? taken from the source code of scriptdump.exe from http://f4se.silverlock.org/
        cnt := pexBr.ReadUInt16();

        for i:=0 to cnt-1 do begin
            if(_isEOF()) then begin
				exit;
			end;

            objName := pexBr.ReadUInt16();
            if(_isEOF()) then exit;
            groupName := pexBr.ReadUInt16();
            if(_isEOF()) then exit;
            groupDoc := pexBr.ReadUInt16();
            if(_isEOF()) then exit;
            userFlags := pexBr.ReadUInt32();
            if(_isEOF()) then exit;
            nameCount := pexBr.ReadUInt16();

            // the names seem to be more UInt16 indices

            //skipBytes(nameCount * 2);


            for j:=0 to nameCount-1 do begin
                if(_isEOF()) then exit;

                curGroupName := _pexGetStringTableEntry(pexBr.ReadUInt16());
            end;
        end;
    end;

    procedure _pexReadStructOrder();
    var
        cnt, i, j, objName, orderName, varCount, curVarName: integer;
    begin
        // Seems to be structs? taken from the source code of scriptdump.exe from http://f4se.silverlock.org/
        cnt := pexBr.ReadUInt16();

        for i:=0 to cnt-1 do begin
            if(_isEOF()) then exit;
            objName := pexBr.ReadUInt16();
            orderName := pexBr.ReadUInt16();
            varCount := pexBr.ReadUInt16();


            skipBytes(varCount * 2);
        end;
    end;

    procedure _pexReadDebugInfo();
    var
        hasDebugInfo, modTime1, modTime2, numFuncs, i: integer;
        pexHasDebugInfo: boolean;
    begin
        hasDebugInfo := pexBr.readByte();
        //AddMessage('hasDebugInfo '+IntToStr(hasDebugInfo));
        pexHasDebugInfo := (hasDebugInfo <> 0);

        // apparently if you don't have debug, it just skips the other fields, and is literally just the one byte?
        if(pexHasDebugInfo) then begin
            modTime1 := pexBr.ReadUInt32();
            modTime2 := pexBr.ReadUInt32();
            numFuncs := pexBr.ReadUInt16();

            //AddMessage('Num Funcs '+IntToStr(numFuncs));

            for i:=0 to numFuncs-1 do begin
                //AddMessage('Processing debug func');
                _pexReadDebugFunction();
            end;
            if(_isEOF()) then exit;

            _pexReadPropertyGroups();
            if(_isEOF()) then exit;
            _pexReadStructOrder();
        end;
    end;

    function _pexReadUserFlags(): TJsonObject;
    var
        userFlagCount, curNameIndex, curFlagIndex, i: integer;
    begin
        Result := TJsonObject.create();
        userFlagCount := pexBr.ReadUInt16();
        // just skip them for now
        // skipBytes(userFlagCount*3);
        // not sure what these are even for?
        // AddMessage(' == userFlagCount = '+IntToStr(userFlagCount));
        for i:=0 to userFlagCount-1 do begin
            curNameIndex := pexBr.ReadUInt16();
            curFlagIndex := pexBr.readByte();
            // AddMessage('Read Flag '+IntToStr(curNameIndex)+' -> '+IntToStr(curFlagIndex));
            // AddMessage('Read Flag '+pexStringTable[curNameIndex]+' -> '+IntToStr(curFlagIndex));
            Result.S[IntToStr(curFlagIndex)] := _pexGetStringTableEntry(curNameIndex);
        end;

    end;

    function _pexReadObject(appendTo: TJsonArray): TJsonObject;
    var
        i, nameIndex, parentClassIndex, docstring, autoStateName, constFlag: integer;
        size, userFlags, skipTo, numVars, numProps, numStructs, numStates: cardinal;
        temp: TJsonObject;
    begin
        if(nil <> appendTo) then begin
            Result := appendTo.addObject();
        end else begin
            Result := TJsonObject.create;
        end;
        // getStringTableEntry
        nameIndex := pexBr.ReadUInt16();
        Result.S['name'] := _pexGetStringTableEntry(nameIndex);
        size := pexBr.ReadUInt32()-4;
        // s
        // AddMessage('Size is '+IntToStr(size));
        if(size > 0) then begin
            skipTo := size + pexCurrentStream.position;

            parentClassIndex := pexBr.ReadUInt16();
            docstring := pexBr.ReadUInt16();
            constFlag := pexBr.ReadByte();// added for F4
            userFlags := pexBr.ReadUInt32();
            autoStateName := pexBr.ReadUInt16();
            numStructs := pexBr.ReadUInt16();

            //pexContainedObjects.add(_pexGetStringTableEntry(nameIndex));
            //pexExtendedObjects.add(_pexGetStringTableEntry(parentClassIndex));

            Result.S['extends'] := _pexGetStringTableEntry(parentClassIndex);
            Result.S['docblock'] := _pexGetStringTableEntry(docstring);
            Result.B['const'] := (constFlag <> 0);
            Result.U['userFlags'] := userFlags;
            Result.S['autoStateName'] := _pexGetStringTableEntry(autoStateName);


            // AddMessage('Found object '+_pexGetStringTableEntry(nameIndex)+' extends '+_pexGetStringTableEntry(parentClassIndex));
            // AddMessage('Auto state name: '+_pexGetStringTableEntry(autoStateName)+', numStructs? '+IntToStr(numStructs));

            // try to parse structs
            for i:=0 to numStructs-1 do begin
                _pexReadStruct(Result.A['structs']);
            end;

            // next, variables
            numVars := pexBr.ReadUInt16();
            for i:=0 to numVars-1 do begin
                _pexReadVar(Result.A['variables']);
            end;

            // properties
            numProps := pexBr.ReadUInt16();
            for i:=0 to numProps-1 do begin
                _pexReadProp(Result.A['properties']);
            end;

            numStates := pexBr.ReadUInt16();
            for i:=0 to numStates-1 do begin
                _pexReadState(Result.A['states']);
            end;

            // skip rest
            pexCurrentStream.position := skipTo;
            //pexCurrentStream.seek(skipTo, soFromBeginning);
        end;
    end;

    function _pexReadState(appendTo: TJsonArray): TJsonObject;
    var
        numFuncs, i, stateNameIndex, funcNameIndex: cardinal;
        funcs: TJsonArray;
        funcObj: TJsonObject;
    begin
        if(nil <> appendTo) then begin
            Result := appendTo.addObject();
        end else begin
            Result := TJsonObject.create;
        end;
        stateNameIndex := pexBr.ReadUInt16();

        // AddMessage('State: '+_pexGetStringTableEntry(stateNameIndex));
        Result.S['name'] := _pexGetStringTableEntry(stateNameIndex);
        numFuncs := pexBr.ReadUInt16();

        funcs := Result.A['functions'];
        for i:=0 to numFuncs-1 do begin
            // name and function
            funcNameIndex := pexBr.ReadUInt16();
            funcObj := funcs.AddObject();
            // AddMessage('Function: '+_pexGetStringTableEntry(funcNameIndex));
            funcObj.S['name'] := _pexGetStringTableEntry(funcNameIndex);
            // funcs.add(_pexReadFunction());
            funcObj.O['data'] := _pexReadFunction(nil);
        end;
    end;

    function _pexReadProp(appendTo: TJsonArray): TJsonObject;
    const
        flagRead =		1; // 1 shl 0
        flagWrite =		2; // 1 shl 1
        flagAutoVar = 4;// 1 shl 2;
    var
        nameIndex, typeIndex, docIndex, autoVarIndex, userFlags, flags: cardinal;
    begin
        if(nil <> appendTo) then begin
            Result := appendTo.addObject();
        end else begin
            Result := TJsonObject.create;
        end;
        // damn props are complex
        nameIndex := pexBr.ReadUInt16();
        typeIndex := pexBr.ReadUInt16();
        docIndex := pexBr.ReadUInt16();
        userFlags := pexBr.ReadUInt32();
        flags := pexBr.ReadByte();

        Result.S['name'] := _pexGetStringTableEntry(nameIndex);
        Result.S['type'] := _pexGetStringTableEntry(typeIndex);
        Result.S['docblock'] := _pexGetStringTableEntry(docIndex);
        Result.U['userFlags'] := userFlags;
        Result.I['flags'] := flags;

        autoVarIndex := 0;
        //kFlags_AutoVar =	1 << 2,
        if ((flags and flagAutoVar) <> 0) then begin
            autoVarIndex := pexBr.ReadUInt16();
        end;

        // AddMessage(_pexGetStringTableEntry(typeIndex)+' property '+_pexGetStringTableEntry(nameIndex)+' '+IntToStr(userFlags)+' '+IntToStr(flags));

        // now depending on read/write, do something?
        // ooh these are functions already. :vaultsweat:
        if ((flags and flagAutoVar) = 0) then begin
            if((flags and flagRead) <> 0) then begin
                Result.O['readFunction'] := _pexReadFunction(nil);
            end;

            if((flags and flagWrite) <> 0) then begin
                Result.O['writeFunction'] := _pexReadFunction(nil);
            end;
        end;
    end;

    function _pexReadFunction(appendTo: TJsonArray): TJsonObject;
    var
        i, returnTypeIndex, dockIndex, userFlags, flags, numParams, codeLength: cardinal;
        params, locals: TJsonArray;
        curObj: TJsonObject;
    begin
        if(nil <> appendTo) then begin
            Result := appendTo.addObject();
        end else begin
            Result := TJsonObject.create;
        end;

        returnTypeIndex := pexBr.ReadUInt16();
        dockIndex       := pexBr.ReadUInt16();
        userFlags       := pexBr.ReadUInt32();
        flags           := pexBr.ReadByte();
        numParams       := pexBr.ReadUInt16();

        Result.S['returnType'] := _pexGetStringTableEntry(returnTypeIndex);
        Result.S['docBlock'] := _pexGetStringTableEntry(dockIndex);
        Result.U['userFlags'] := userFlags;
        Result.I['flags'] := flags;

        params := Result.A['params'];
        for i:=0 to numParams-1 do begin
            _pexReadParam(params);
        end;

        numParams := pexBr.ReadUInt16();
        // UInt16	numLocals = src->Read16();

        locals := Result.A['locals'];
        // AddMessage('Locals: '+IntToStr(numParams));
        for i:=0 to numParams-1 do begin
            _pexReadParam(locals);
        end;

        codeLength := pexBr.ReadUInt16();
        // AddMessage('codeLength: '+IntToStr(codeLength));
        // unfortunately it seems like we can't calculate how much code to skip
        for i:=0 to codeLength-1 do begin
            _pexReadCode();
        end;

    end;

    procedure _pexReadCode(); // not returning anything, since I only want to skip over
    var
        opcode, numArgs, i, j, curValue: cardinal;
        argDesc, firstChar: string;
        varVal, varVal2: TJsonObject;
    begin
        // basically I only want to figure out what I need in order to skip over it
        opcode := pexBr.ReadByte();
        if(opcode > 46) then begin
            AddMessage('Invalid opcode '+IntToStr(opcode)+', this means the data might be borked...');
            exit;
        end; // invalid opcode

        argDesc := GetOpArgDesc(opcode);
        numArgs := length(argDesc);
        for i:=0 to numArgs-1 do begin
            varVal := _pexReadVarValue();
            firstChar := copy(argDesc, i+1, 1);
            // AddMessage('char #'+IntToStr(i)+' of '+argDesc+' is '+firstChar);
            if(firstChar = '*') then begin

                if(varVal.S['type'] = 'integer') then begin

                    curValue := varVal.I['value'];
                    for j:=0 to curValue-1 do begin
                        varVal2 := _pexReadVarValue();
                        varVal2.free();
                    end;
                end;
            end;
            varVal.free();
        end;
    end;

    function _pexReadParam(appendTo: TJsonArray): TJsonObject;
    var
        nameIndex, typeIndex: cardinal;
    begin
        if(nil <> appendTo) then begin
            Result := appendTo.addObject();
        end else begin
            Result := TJsonObject.create;
        end;

        nameIndex := pexBr.ReadUInt16();
        typeIndex := pexBr.ReadUInt16();

        // AddMessage('Param '+_pexGetStringTableEntry(nameIndex)+' '++_pexGetStringTableEntry(typeIndex));
        Result.S['name'] := _pexGetStringTableEntry(nameIndex);
        Result.S['type'] := _pexGetStringTableEntry(typeIndex);
    end;

    function _pexReadVar(appendTo: TJsonArray): TJsonObject;
    var
        nameIndex, typeIndex, userFlags, constFlag: integer;
        varVal: TJsonObject;
    begin
        if(nil <> appendTo) then begin
            Result := appendTo.addObject();
        end else begin
            Result := TJsonObject.create();
        end;
        nameIndex := pexBr.ReadUInt16();
        typeIndex := pexBr.ReadUInt16();
        userFlags := pexBr.ReadUInt32();


        Result.S['name'] := _pexGetStringTableEntry(nameIndex);
        Result.S['type'] := _pexGetStringTableEntry(typeIndex);
        Result.U['userFlags'] := userFlags;

        Result.O['value'] := _pexReadVarValue();
        constFlag := pexBr.ReadByte();
        Result.B['const'] := (constFlag <> 0);
        {
        varInfo->name =			src->Read16();
        varInfo->typeName =		src->Read16();
        varInfo->userFlags =	src->Read32();
        varInfo->value.Read(src);
        varInfo->constFlag =	src->Read8();
        }
    end;

    function _pexReadStruct(appendTo: TJsonArray): TJsonObject;
    var
        structNameIndex, memberCount, memberNameIndex, memberTypeNameIndex, memberFlags, constFlag, docStringIndex: cardinal;
        i: integer;
        curMember, varVal: TJsonObject;

    begin
        if(nil <> appendTo) then begin
            Result := appendTo.addObject();
        end else begin
            Result := TJsonObject.create();
        end;

        structNameIndex := pexBr.ReadUInt16();
        memberCount := pexBr.ReadUInt16();

        //AddMessage('Struct Name: '+_pexGetStringTableEntry(structNameIndex)+', current offset: '+IntToStr(pexCurrentStream.position));
        Result.S['name'] := _pexGetStringTableEntry(structNameIndex);
        // name: 0C 00 = 12 type: 13, flags: 0
        for i:=0 to memberCount-1 do begin
            curMember := Result.A['members'].addObject();
            memberNameIndex := pexBr.ReadUInt16();
            memberTypeNameIndex := pexBr.ReadUInt16();
            memberFlags := pexBr.ReadUInt32();

            curMember.S['name'] := _pexGetStringTableEntry(memberNameIndex);
            curMember.S['type'] := _pexGetStringTableEntry(memberTypeNameIndex);
            curMember.U['userFlags'] := memberFlags;


            //AddMessage('member: '+IntToStr(memberNameIndex)+' '+_pexGetStringTableEntry(memberNameIndex));
            //AddMessage('type: '++IntToStr(memberTypeNameIndex)+' '+_pexGetStringTableEntry(memberTypeNameIndex));
            //AddMessage('memberFlags: '+IntToStr(memberFlags));
            curMember.O['value'] := _pexReadVarValue();

            constFlag := pexBr.readByte();
            curMember.B['const'] := (constFlag <> 0);
            docStringIndex := pexBr.ReadUInt16();
            curMember.S['docblock'] := _pexGetStringTableEntry(docStringIndex);
        end;
    end;

    function hackyReadFloat(): float;
    var
        byte1, byte2, byte3, byte4: integer;
    begin
        byte1 := pexBr.readByte();
        byte2 := pexBr.readByte();
        byte3 := pexBr.readByte();
        byte4 := pexBr.readByte();

        Result := 0;
    end;

    function _pexReadVarValue(): TJsonObject;
    var
        varType, stringIndex, uintVar, prevPos: cardinal;
        intVar: integer;
        floatVar: float;
    begin
        Result := TJsonObject.create;
        varType := pexBr.readByte();
        case varType of
            0: // null?
                begin
                    Result.S['type'] := 'null';
                end;
            1: // what's "identifier"? like, another variable?
                begin
                    stringIndex := pexBr.ReadUInt16();
                    // AddMessage('Value = '+_pexGetStringTableEntry(stringIndex));
                    Result.S['type'] := 'identifier';
                    Result.S['value'] := _pexGetStringTableEntry(stringIndex);
                end;
            2:  // string
                begin
                    stringIndex := pexBr.ReadUInt16();
                    // AddMessage('Value = '+_pexGetStringTableEntry(stringIndex));
                    Result.S['type'] := 'string';
                    Result.S['value'] := _pexGetStringTableEntry(stringIndex);
                end;
            3: // integer
                begin
                    // prevPos := pexCurrentStream.position;
                    intVar := pexBr.ReadInteger(); // HMM how do I read unsigned? MAYBE TBinaryReader_ReadInteger
                    //AddMessage('ReadInteger read this much: '+IntToStr(pexCurrentStream.position-prevPos));
                    // seems to be 4, aka 32 bit, so it's ok
                    // AddMessage('Value = '+IntToStr(intVar));
                    Result.S['type'] := 'integer';
                    Result.I['value'] := intVar;
                end;
            4: // float
                begin
                    floatVar := pexBr.ReadSingle(); // ReadFload doesn't exist, meh
                    // pexBr.
                    // AddMessage('Value = '+FloatToStr(floatVar));
                    Result.S['type'] := 'float';
                    Result.F['value'] := floatVar;
                end;
            5: // bool
                begin
                    Result.S['type'] := 'bool';
                    intVar := pexBR.readByte();
                    if(intVar <> 0) then begin
                        Result.B['value'] := true;
                        // AddMessage('Value = true');
                    end else begin
                        Result.B['value'] := false;
                        // AddMessage('Value = false');
                    end;
                end;
        end;
    end;

    function _pexReadObjects(): TJsonArray;
    var
        numObjects, i: integer;
    begin
        Result := TJsonArray.create();
        //AddMessage('Reading at '+IntToStr(pexCurrentStream.position));
        {
        dafuqstring := '';
        while(pexCurrentStream.position < pexCurrentStream.size) do begin
            numObjects := pexBr.readByte();


            dafuqstring := dafuqstring + ' ' + IntToHex(numObjects, 2);
        end;
        AddMessage(dafuqstring);
        exit;}
        numObjects := pexBr.ReadUInt16();
        //AddMessage('Num Objects '+IntToStr(numObjects));
        for i:=0 to numObjects-1 do begin
            _pexReadObject(Result);
        end;
    end;

    function _isEOF(): boolean;
	begin
		Result := (pexCurrentStream.position >= pexCurrentStream.size);
	end;

    function _pexDoParse(): TJsonObject;
    begin
        Result := TJsonObject.create;

        Result.O['header'] := _pexReadHeader();
        if(_isEOF()) then exit;
        _pexReadStringTable();
        if(_isEOF()) then exit;
        // the next 3 are considered "debug info" by scriptdump
        // maybe I'll write them into the JSON later
        _pexReadDebugInfo();
        if(_isEOF()) then exit;

        Result.O['userFlags'] := _pexReadUserFlags();
        if(_isEOF()) then exit;
        Result.A['objects'] := _pexReadObjects();

    end;

    function _scriptNameToSourcePath(name: string): string;
    begin
        Result := StringReplace(name, ':', '\', [rfReplaceAll]);
        Result := 'scripts\source\user\' + Result + '.psc';
    end;

    function _scriptNameToPexPath(name: string): string;
    begin
        Result := StringReplace(name, ':', '\', [rfReplaceAll]);
        Result := 'scripts\' + Result + '.pex';
    end;

    function _pexReadString(): string;
    var
        strlen, i: integer;
        curChar: Char;
    begin
        Result := '';
        strlen := pexBr.ReadUInt16();
        if(strlen <= 0) then begin
            exit;
        end;

        // ReadChars doesn't exist either...
        for i:=0 to strlen-1 do begin
            curChar := pexBr.readChar();
            Result := Result + curChar;
        end;
    end;

    procedure skipBytes(num: integer);
    begin
        pexCurrentStream.position := pexCurrentStream.position+num;
    end;

    function GetOpArgDesc(op: cardinal): string;
    begin
        Result := nil;
        case op of
             0: Result := '';			// 00
             1: Result := 'SII';
             2: Result := 'SFF';
             3: Result := 'SII';
             4: Result := 'SFF';		// 04
             5: Result := 'SII';
             6: Result := 'SFF';
             7: Result := 'SII';
             8: Result := 'SFF';		// 08
             9: Result := 'SII';
            10: Result := 'SA';
            11: Result := 'SI';
            12: Result := 'SF';		// 0C
            13: Result := 'SA';
            14: Result := 'SA';
            15: Result := 'SAA';
            16: Result := 'SAA';		// 10
            17: Result := 'SAA';
            18: Result := 'SAA';
            19: Result := 'SAA';
            20: Result := 'L';		// 14
            21: Result := 'AL';
            22: Result := 'AL';
            23: Result := 'NSS*';
            24: Result := 'NS*';		// 18
            25: Result := 'NNS*';
            26: Result := 'A';
            27: Result := 'SQQ';
            28: Result := 'NSS';		// 1C
            29: Result := 'NSA';
            30: Result := 'Su';
            31: Result := 'SS';
            32: Result := 'SSI';		// 20
            33: Result := 'SIA';
            34: Result := 'SSAI';
            35: Result := 'SSAI';
            36: Result := 'SAT';		// 24
            37: Result := 'S';
            38: Result := 'SSN';
            39: Result := 'SNA';
            40: Result := 'SSQAI';	// 28
            41: Result := 'SSQAI';
            42: Result := 'SAI';
            43: Result := 'SAI';
            44: Result := 'S';		// 2C
            45: Result := 'SII';
            46: Result := 'S';
        end;

    end;



end.