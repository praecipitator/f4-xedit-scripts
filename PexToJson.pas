{
    Poor man's PEX "decompiler" (more of a disassembler)
    Version 2.0

    Sources:
        - http://f4se.silverlock.org/
            The source code of scriptdump was the basis for most of this.
        - https://en.uesp.net/wiki/Tes5Mod:Compiled_Script_File_Format
            More of a honorable mention, since it's for Skyrim, which is sufficiently different.

    Usage:
        - Call either readPexResource, pexReadFile, or pexReadStream. It will return a TJsonObject. Clean it up manually.

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
        - "debug": object
            - "timestamp": uint
                The unix timestamp of when this was compiled.
            - "functions": array of PEXDebugFunction
            - "groups": array of PEXDebugGroup
            - "structs: array of PEXDebugStruct
        - "userFlags": object
            Contains this:
                "0": "hidden",
                "1": "conditional",
                "2": "default",
                "3": "collapsedonref",
                "4": "collapsedonbase",
                "5": "mandatory"
            The 0-5 numbers are the leftshift argument, that is, "5" for "mandatory" means it's actually 1 shl 5 = 32.
            These are also represented by the PEX_FLAG_* constants.
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
            If no auto state is defined, this will be "".
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
            This represents the default value of the variable.
        - "const": boolean
        - "docblock": string

    PEXVariable structure:
        - "name": string
            If this begins with "::" and ends with "_var", then it's an auto-generated local variable of a property.
        - "type": string
        - "userFlags": int
        - "value": PEXValue
            This represents the default value of the variable.
        - "const": bool

    PEXProperty structure:
        - "name": string
        - "type": string
        - "docblock": string
        - "userFlags": int
        - "flags": int

    PEXState structure:
        - "name": string
            This is "" for the "empty state".
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
        - "code": array of PEXCodeValue

    PEXCode structure:
        - "op": string
            One of "nop", "iadd", "fadd", "isub", "fsub", "imul", "fmul", "idiv", "fdiv", "imod", "not", "ineg", "fneg", "assign", "cast", "cmp_eq", "cmp_lt", "cmp_le", "cmp_gt", "cmp_ge", "jmp", "jmpt", "jmpf", "callmethod", "callparent", "callstatic", "return", "strcat", "propget", "propset", "array_create", "array_length", "array_getelement", "array_setelement", "array_findelement", "array_rfindelement", "is", "struct_create", "struct_get", "struct_set", "array_findstruct", "array_rfindstruct", "array_add", "array_insert", "array_removelast", "array_remove", "array_clear".
        - "args": array of PEXValue

    PEXParam structure:
        - "name": string
        - "type": string
            The actual type as written in the script, custom types will be here, too.

    PEXValue structure:
        - "name": string
        - "type": string
            One of "null", "identifier", "string", "integer", "float", "bool".

    PEXCodeValue structure:
        - "name": string
        - "type": string
            One of "null", "identifier", "string", "integer", "float", "bool".
        - "argCode": string
            One of "S", "L", "I", "F", "A", "Q", "u", "N", "T", "*".
            
            
    PEXDebugFunction structure:
        - "object": string
            The object this function belongs to, should be the same as the script name.
        - "state": string
            The state in which this function is defined, emptystring if no state.
        - "function": string
            The name of the function.
        - "type": int
            Unknown.
        - "lineNumbers": array of integer
            Seems to be line numbers of the original source file, in the same order as the corresponding instructions in PEXFunction's "code" array?
        
    PEXDebugGroup structure:
        - "object": string
            The object this group belongs to, should be the same as the script name.
        - "group": string
            Name of the property group. Can be empty, as it seems like the default "empty" group is also added here.
        - "docblock": string
            The docblock string of this group.
        - "flags": uint
            Flags of the group. "collapsed" seems to be 24
        - "properties": array of string
            Names of the properties within this group, in the "proper" order.
            
    PEXDebugStruct structure:
        - "object": string
            The object this struct belongs to, should be the same as the script name.
        - "struct": string
            Name of this struct.
        - "members": array of string
            Names of the struct members, in the "proper" order.
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
        scriptTypeBlackList: TStringList;
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

    {
        analyzes a previously parsed pex, and returns a list of all script names which are used in it.
    }
    function getUsedScripts(pexJson: TJsonObject): TStringList;
    var
        i: integer;
        curObj: TJsonObject;
    begin
        createBlackList();
        Result := TStringList.create;
        Result.Duplicates := dupIgnore;
        Result.CaseSensitive := false;
        Result.Sorted := true;
        for i:=0 to pexJson.A['objects'].count-1 do begin
            curObj := pexJson.A['objects'].O[i];

            addScriptToList(Result, curObj.S['extends']);
            // structs
            addScriptsFromStructs(Result, curObj.A['structs']);
            // variables
            addScriptsFromVariables(Result, curObj.A['variables']);
            // props, they are similar enough
            addScriptsFromVariables(Result, curObj.A['properties']);

            // the hard part, aka states
            addScriptsFromStates(Result, curObj.A['states']);

        end;
        cleanupBlackList();
    end;

    // ==========================================================================
    // === END of "public" functions. Please do not call anything below manually.
    // ==========================================================================

    procedure createBlackList();
    begin
        scriptTypeBlackList := TStringList.create;
        scriptTypeBlackList.Duplicates := dupIgnore;
        scriptTypeBlackList.CaseSensitive := false;
        scriptTypeBlackList.Sorted := true;

        scriptTypeBlackList.add('float');
        scriptTypeBlackList.add('int');
        scriptTypeBlackList.add('string');
        scriptTypeBlackList.add('bool');
        scriptTypeBlackList.add('var');
        scriptTypeBlackList.add('none');
        scriptTypeBlackList.add('debug');
        scriptTypeBlackList.add('f4se');
        scriptTypeBlackList.add('game');
        scriptTypeBlackList.add('input');
        scriptTypeBlackList.add('inputenablelayer');
        scriptTypeBlackList.add('instancedata');
        scriptTypeBlackList.add('math');
        scriptTypeBlackList.add('ui');
        scriptTypeBlackList.add('utility');
        scriptTypeBlackList.add('shout');
        scriptTypeBlackList.add('wordofpower');
        scriptTypeBlackList.add('leveledspell');
        scriptTypeBlackList.add('scroll');
        scriptTypeBlackList.add('soulgem');
        scriptTypeBlackList.add('scriptobject');
        scriptTypeBlackList.add('form');
        scriptTypeBlackList.add('objectreference');
        scriptTypeBlackList.add('actor');
        scriptTypeBlackList.add('alias');
        scriptTypeBlackList.add('referencealias');
        scriptTypeBlackList.add('locationalias');
        scriptTypeBlackList.add('refcollectionalias');
        scriptTypeBlackList.add('activemagiceffect');
        scriptTypeBlackList.add('action');
        scriptTypeBlackList.add('activator');
        scriptTypeBlackList.add('flora');
        scriptTypeBlackList.add('furniture');
        scriptTypeBlackList.add('talkingactivator');
        scriptTypeBlackList.add('actorbase');
        scriptTypeBlackList.add('actorvalue');
        scriptTypeBlackList.add('ammo');
        scriptTypeBlackList.add('armor');
        scriptTypeBlackList.add('associationtype');
        scriptTypeBlackList.add('book');
        scriptTypeBlackList.add('camerashot');
        scriptTypeBlackList.add('cell');
        scriptTypeBlackList.add('class');
        scriptTypeBlackList.add('combatstyle');
        scriptTypeBlackList.add('component');
        scriptTypeBlackList.add('container');
        scriptTypeBlackList.add('door');
        scriptTypeBlackList.add('defaultobject');
        scriptTypeBlackList.add('effectshader');
        scriptTypeBlackList.add('enchantment');
        scriptTypeBlackList.add('encounterzone');
        scriptTypeBlackList.add('equipslot');
        scriptTypeBlackList.add('explosion');
        scriptTypeBlackList.add('faction');
        scriptTypeBlackList.add('formlist');
        scriptTypeBlackList.add('globalvariable');
        scriptTypeBlackList.add('hazard');
        scriptTypeBlackList.add('headpart');
        scriptTypeBlackList.add('holotape');
        scriptTypeBlackList.add('idle');
        scriptTypeBlackList.add('idlemarker');
        scriptTypeBlackList.add('imagespacemodifier');
        scriptTypeBlackList.add('impactdataset');
        scriptTypeBlackList.add('ingredient');
        scriptTypeBlackList.add('instancenamingrules');
        scriptTypeBlackList.add('keyword');
        scriptTypeBlackList.add('locationreftype');
        scriptTypeBlackList.add('leveledactor');
        scriptTypeBlackList.add('leveleditem');
        scriptTypeBlackList.add('leveledspell');
        scriptTypeBlackList.add('light');
        scriptTypeBlackList.add('location');
        scriptTypeBlackList.add('magiceffect');
        scriptTypeBlackList.add('message');
        scriptTypeBlackList.add('miscobject');
        scriptTypeBlackList.add('constructibleobject');
        scriptTypeBlackList.add('key');
        scriptTypeBlackList.add('soulgem');
        scriptTypeBlackList.add('musictype');
        scriptTypeBlackList.add('objectmod');
        scriptTypeBlackList.add('outfit');
        scriptTypeBlackList.add('outputmodel');
        scriptTypeBlackList.add('package');
        scriptTypeBlackList.add('perk');
        scriptTypeBlackList.add('potion');
        scriptTypeBlackList.add('projectile');
        scriptTypeBlackList.add('quest');
        scriptTypeBlackList.add('race');
        scriptTypeBlackList.add('scene');
        scriptTypeBlackList.add('scroll');
        scriptTypeBlackList.add('shaderparticlegeometry');
        scriptTypeBlackList.add('shout');
        scriptTypeBlackList.add('sound');
        scriptTypeBlackList.add('soundcategory');
        scriptTypeBlackList.add('soundcategorysnapshot');
        scriptTypeBlackList.add('spell');
        scriptTypeBlackList.add('static');
        scriptTypeBlackList.add('movablestatic');
        scriptTypeBlackList.add('terminal');
        scriptTypeBlackList.add('textureset');
        scriptTypeBlackList.add('topic');
        scriptTypeBlackList.add('topicinfo');
        scriptTypeBlackList.add('visualeffect');
        scriptTypeBlackList.add('voicetype');
        scriptTypeBlackList.add('watertype');
        scriptTypeBlackList.add('weapon');
        scriptTypeBlackList.add('weather');
        scriptTypeBlackList.add('wordofpower');
        scriptTypeBlackList.add('worldspace');
    end;

    procedure cleanupBlackList();
    begin
        scriptTypeBlackList.free();
        scriptTypeBlackList := nil;
    end;

    function isBuiltInType(typeStr: string): boolean;
    var
        typeLc: string;
    begin
        Result := (scriptTypeBlackList.indexOf(typeStr) >= 0);


    end;

    procedure addScriptToList(outList: TStringList; typeStr: string);
    var
        prefix, suffix: string;
        hashPos: integer;
    begin

        if (typeStr = '') or isBuiltInType(typeStr) then begin
            exit;
        end;

        suffix := copy(typeStr, length(typeStr)-1, 2); // returns the last two chars
        if(suffix = '[]') then begin
            prefix := copy(typeStr, 1, length(typeStr)-2); // cuts off the last two chars

            addScriptToList(outList, prefix);
            exit;

            //if (prefix = '') or isBuiltInType(prefix) then begin
                //exit;
            //end;
            //outList.Add(prefix);
        end;

        hashPos := Pos('#', typeStr);
        if(hashPos > 0) then begin
            // cut it
            prefix := copy(typeStr, 1, hashPos-1);
            addScriptToList(outList, prefix);
            exit;
        end;

        outList.Add(typeStr);
    end;

    procedure addScriptsFromStates(outList: TStringList; states: TJsonArray);
    var
        i, j: integer;
        curFunc: TJsonObject;
        functions: TJsonArray;
    begin
        for i:=0 to states.count-1 do begin

            functions := states.O[i].A['functions'];
            for j:=0 to functions.count-1 do begin
                curFunc := functions.O[j];

                // params
                addScriptsFromVariables(outList, curFunc.O['data'].A['params']);
                // locals
                addScriptsFromVariables(outList, curFunc.O['data'].A['locals']);
                // code
                addScriptsFromCode(outList, curFunc.O['data'].A['code']);
            end;
        end;
    end;

    procedure addScriptsFromCode(outList: TStringList; code: TJsonArray);
    var
        i: integer;
        curEntry, curArg: TJsonObject;
        curOp: string;
    begin
        for i:=0 to code.count-1 do begin
            curEntry := code.O[i];
            curOp := curEntry.S['op'];
            // only check "callstatic" and "is"
            if(curOp = 'callstatic') then begin
                //AddMessage('Found a callstatic');
                // NSS* -> the first N arg is the name
                curArg := curEntry.A['args'].O[0];
                addScriptToList(outList, curArg.S['value']);
            end else if (curOp = 'is') then begin
                // SAT -> third T arg
                curArg := curEntry.A['args'].O[2];
                addScriptToList(outList, curArg.S['value']);
            end;
        end;
    end;

    procedure addScriptsFromVariables(outList: TStringList; vars: TJsonArray);
    var
        i: integer;
        members: TJsonArray;
        curType: string;
    begin
        for i:=0 to vars.count-1 do begin
            curType := vars.O[i].S['type'];
            addScriptToList(outList, curType);
        end;
    end;

    // for getUsedScripts begin
    procedure addScriptsFromStructs(outList: TStringList; structs: TJsonArray);
    var
        i, j: integer;
        members: TJsonArray;
        curType: string;
    begin
        for i:=0 to structs.count-1 do begin
            // we care about the types of the members
            members := structs.O[i].A['members'];
            for j:=0 to members.count-1 do begin
                curType := members.O[j].S['type'];
                addScriptToList(outList, curType);
            end;
        end;
    end;
    // for getUsedScripts end

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

    function _pexReadDebugFunction(appendTo: TJsonArray): TJsonObject;
    var
        objectNameIndex, stateNameIndex, functionNameIndex, functionType, instructionCount, i: integer;
        instructions: TJsonArray;
    begin
        if(nil <> appendTo) then begin
            Result := appendTo.addObject();
        end else begin
            Result := TJsonObject.create;
        end;


        objectNameIndex := pexBr.ReadUInt16();
        stateNameIndex  := pexBr.ReadUInt16();
        functionNameIndex := pexBr.ReadUInt16();
        functionType := pexBr.readByte();
        instructionCount := pexBr.ReadUInt16();

        //skipBytes(instructionCount * 2);
        Result.S['object'] := _pexGetStringTableEntry(objectNameIndex);
        Result.S['state'] := _pexGetStringTableEntry(stateNameIndex);
        Result.S['function'] := _pexGetStringTableEntry(functionNameIndex);
        Result.I['type'] := IntToStr(functionType);

        instructions := Result.A['lineNumbers'];

        // AddMessage('objectNameIndex '+pexStringTable[objectNameIndex]+', stateNameIndex '+pexStringTable[stateNameIndex]+', functionNameIndex '+pexStringTable[functionNameIndex]);
        //AddMessage('NumInstr '+IntToStr(instructionCount));
        for i:=0 to instructionCount-1 do begin
            instructions.add(pexBr.ReadUInt16());
            // potentially do something with the info? or just skip 2*instructionCount right away?
            // AddMessage('    Line NR: '+IntToStr(pexBr.ReadUInt16()));
        end;

    end;

    function _pexReadPropertyGroups(): TJsonArray;
    var
        cnt, objName, groupName, groupDoc, nameCount, i, j: integer;
        userFlags: cardinal;
        curGroupName: string;
        curEntry: TJsonObject;
        propNames: TJsonArray;
    begin
        Result := TJsonArray.create;
        // Seems to be Groups? taken from the source code of scriptdump.exe from http://f4se.silverlock.org/
        cnt := pexBr.ReadUInt16();

        for i:=0 to cnt-1 do begin
            if(_isEOF()) then begin
				exit;
			end;

            // the names seem to be more UInt16 indices
            objName := pexBr.ReadUInt16();
            if(_isEOF()) then exit;
            groupName := pexBr.ReadUInt16();
            if(_isEOF()) then exit;
            groupDoc := pexBr.ReadUInt16();
            if(_isEOF()) then exit;
            userFlags := pexBr.ReadUInt32();
            if(_isEOF()) then exit;
            nameCount := pexBr.ReadUInt16();

            curEntry := Result.addObject();
            curEntry.S['object'] := _pexGetStringTableEntry(objName);
            curEntry.S['group'] := _pexGetStringTableEntry(groupName);
            curEntry.S['docblock'] := _pexGetStringTableEntry(groupDoc);
            curEntry.U['flags'] := userFlags;

            propNames := curEntry.A['properties'];
            for j:=0 to nameCount-1 do begin
                if(_isEOF()) then exit;

                curGroupName := _pexGetStringTableEntry(pexBr.ReadUInt16());
                propNames.add(curGroupName);
            end;
        end;
    end;

    function _pexReadStructOrder(): TJsonArray;
    var
        cnt, i, j, objName, orderName, varCount, curVarName: integer;
        curObj: TJsonObject;
        curNames: TJsonArray;
    begin
        Result := TJsonArray.create;
        // Seems to be structs? taken from the source code of scriptdump.exe from http://f4se.silverlock.org/
        cnt := pexBr.ReadUInt16();

        for i:=0 to cnt-1 do begin
            if(_isEOF()) then exit;
            curObj := Result.addObject();

            objName := pexBr.ReadUInt16();
            orderName := pexBr.ReadUInt16();
            varCount := pexBr.ReadUInt16();

            curObj.S['object'] := _pexGetStringTableEntry(objName);
            curObj.S['struct'] := _pexGetStringTableEntry(orderName);

            // what are these?
            // skipBytes(varCount * 2);
            curNames := curObj.A['members'];
            for j:=0 to varCount-1 do begin
                curVarName := pexBr.ReadUInt16();
                curNames.add(_pexGetStringTableEntry(curVarName));
            end;
        end;
    end;

    function _pexReadDebugInfo(): TJsonObject;
    var
        hasDebugInfo, modTime1, modTime2, numFuncs, i: integer;
        pexHasDebugInfo: boolean;
        modTime: cardinal;
        debugFunctions: TJsonArray;
    begin
        Result := TJsonObject.create();
        hasDebugInfo := pexBr.readByte();
        //AddMessage('hasDebugInfo '+IntToStr(hasDebugInfo));
        pexHasDebugInfo := (hasDebugInfo <> 0);

        // apparently if you don't have debug, it just skips the other fields, and is literally just the one byte?
        if(not pexHasDebugInfo) then begin
            exit;
        end;

        modTime1 := pexBr.ReadUInt32();
        modTime2 := pexBr.ReadUInt32();
        numFuncs := pexBr.ReadUInt16();

        // I think I can just (modTime2 << something) + modTime1, and then it's a timestamp
        // FFFFFFFF
        // expected:  1707253497 0 -> Tue Feb 06 22:04:57 2024

        modTime := (modTime2 shl $FFFFFFFF) + modTime1;
        // AddMessage('ModTime: '+IntToStr(modTime));
        Result.U['timestamp'] := modTime;

        //AddMessage('Num Funcs '+IntToStr(numFuncs));
        debugFunctions := Result.A['functions'];

        for i:=0 to numFuncs-1 do begin
            //AddMessage('Processing debug func');
            _pexReadDebugFunction(debugFunctions);
        end;

        if(_isEOF()) then exit;

        Result.A['groups'] := _pexReadPropertyGroups();
        if(_isEOF()) then exit;
        Result.A['structs'] := _pexReadStructOrder();
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
        params, locals, code: TJsonArray;
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


        code := Result.A['code'];
        codeLength := pexBr.ReadUInt16();
        // AddMessage('codeLength: '+IntToStr(codeLength));
        // unfortunately it seems like we can't calculate how much code to skip
        for i:=0 to codeLength-1 do begin
            _pexReadCode(code);
        end;

    end;

    function _pexReadCode(appendTo: TJsonArray): TJsonObject;
    var
        opcode, numArgs, i, j, curValue: cardinal;
        argDesc, curChar, opName: string;
        varVal, varVal2: TJsonObject;
        args, tempArgs: TJsonArray;
    begin
        if(nil <> appendTo) then begin
            Result := appendTo.addObject();
        end else begin
            Result := TJsonObject.create;
        end;

        opcode := pexBr.ReadByte();
        if(opcode > 46) then begin
            AddMessage('Invalid opcode '+IntToStr(opcode)+', this means the data might be borked...');
            exit;
        end; // invalid opcode

        // so here, we have an opcode, and N arguments.
        // the nr of arguments is length of argDesc, unless one is a *.
        // The * doesn't count as an argument, but it must be of type integer, and it's value
        // is the number of varargs.

        opName := GetOpname(opcode);
        Result.S['op'] := opName;

        args := Result.A['args'];
        argDesc := GetOpArgDesc(opcode);


        // tempArgs := TJsonArray.create;
        numArgs := length(argDesc); // this seems to be the "default" length of arguments
        for i:=0 to numArgs-1 do begin
            varVal := _pexReadVarValue(nil);
            curChar := copy(argDesc, i+1, 1);
            // AddMessage('char #'+IntToStr(i)+' of '+argDesc+' is '+firstChar);
            if(curChar = '*') then begin
                // remove cur arg
                // args.remove(args.count-1);
                if(varVal.S['type'] = 'integer') then begin

                    curValue := varVal.I['value'];
                    for j:=0 to curValue-1 do begin
                        varVal2 := _pexReadVarValue(args);
                        varVal2.S['argCode'] := curChar;
                        //appendObjectToArray(args, varVal2);
                        //varVal2.free();
                    end;
                end;
                varVal.free();
            end else begin
                varVal2 := appendObject(args, varVal);
                varVal2.S['argCode'] := curChar;
                //args.addObject();
                //args.A[args.count-1] := varVal;
            end;
        end;
    end;

    procedure appendObject(arr: TJsonArray; obj: TJsonObject);
    var
        newIndex: integer;
    begin
        newIndex := arr.count;
        arr.addObject();
        arr.O[newIndex]:= obj;
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

        Result.O['value'] := _pexReadVarValue(nil);
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
            curMember.O['value'] := _pexReadVarValue(nil);

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

    function _pexReadVarValue(appendTo: TJsonArray): TJsonObject;
    var
        varType, stringIndex, uintVar, prevPos: cardinal;
        intVar: integer;
        floatVar: float;
    begin
        if(nil <> appendTo) then begin
            Result := appendTo.addObject();
        end else begin
            Result := TJsonObject.create;
        end;

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
        Result := TJsonObject.create();

        Result.O['header'] := _pexReadHeader();
        if(_isEOF()) then exit;
        _pexReadStringTable();
        if(_isEOF()) then exit;
        // the next 3 are considered "debug info" by scriptdump
        // maybe I'll write them into the JSON later
        Result.O['debug'] := _pexReadDebugInfo();
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

    function GetOpname(op: cardinal): string;
    begin
        Result := 'invalid';
        case op of
             0: Result := 'nop';
             1: Result := 'iadd';
             2: Result := 'fadd';
             3: Result := 'isub';
             4: Result := 'fsub';
             5: Result := 'imul';
             6: Result := 'fmul';
             7: Result := 'idiv';
             8: Result := 'fdiv';
             9: Result := 'imod';
            10: Result := 'not';
            11: Result := 'ineg';
            12: Result := 'fneg';
            13: Result := 'assign';
            14: Result := 'cast';
            15: Result := 'cmp_eq';
            16: Result := 'cmp_lt';
            17: Result := 'cmp_le';
            18: Result := 'cmp_gt';
            19: Result := 'cmp_ge';
            20: Result := 'jmp';
            21: Result := 'jmpt';
            22: Result := 'jmpf';
            23: Result := 'callmethod';
            24: Result := 'callparent';
            25: Result := 'callstatic';
            26: Result := 'return';
            27: Result := 'strcat';
            28: Result := 'propget';
            29: Result := 'propset';
            30: Result := 'array_create';
            31: Result := 'array_length';
            32: Result := 'array_getelement';
            33: Result := 'array_setelement';
            34: Result := 'array_findelement';
            35: Result := 'array_rfindelement';
            36: Result := 'is';
            37: Result := 'struct_create';
            38: Result := 'struct_get';
            39: Result := 'struct_set';
            40: Result := 'array_findstruct';
            41: Result := 'array_rfindstruct';
            42: Result := 'array_add';
            43: Result := 'array_insert';
            44: Result := 'array_removelast';
            45: Result := 'array_remove';
            46: Result := 'array_clear';
        end;
        {

        }

    end;

    function GetOpArgDesc(op: cardinal): string;
    begin
        Result := nil;
        // S = Subject? or this might be what the op assigns the result to. or operates on? But there are more S than one in some...
        // L = Location to jump to?
        // I = Integer
        // F = Float
        // A = Bool
        // Q = String
        // u = unsigned int? only used by array_create
        // N = Identifier which is something like a function/class/struct name
        // T = Type
        // * = vararg, I think these can only be IFAQN
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