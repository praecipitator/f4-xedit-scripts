{
    Poor man's PEX parser.
    Can get some info, but not much...
    
    Source: https://en.uesp.net/wiki/Tes5Mod:Compiled_Script_File_Format
    (Kinda, that is for Skyrim, F4 is similar, but different)

    the first 16 bytes seem to be fixed. Python unpack_from: "<IBBHQ"
    (everything little endian)
    0-3: unsigned int "magic"
    4: unsigned char major version
    5: unsigned char minor version
    6-7: unsigned short game ID
    8-F: unsigned long long compilation time
    
    after this, 3 strings follow
    strings are prefixed with an unsigned short (two-byte) length
    
    after this, the string table follows.
    first two bytes seems to be the the nr. of strings in the table (unsigned short)
    afterwards, the strings follow, in the aforementioned format.
    
    
}
unit PexParser;
    
    var
        pexMajorVersion, pexMinorVersion, pexGameId: integer;
        pexCompileTime1: cardinal;
        pexCompileTime2: cardinal;
        pexSourceName, pexUserName, pexMachineName: string;
        
        pexContainedObjects, pexExtendedObjects: TStringList;
        
        pexStringTable: TStringList;
        
        pexHasDebugInfo: boolean;

        pexBr: TBinaryReader;
        
        pexCurrentStream: TStream;
        // I'm not going to include actual functions, at least for now
        
    function pexGetStringTableEntry(i: integer): string;
    begin
        if(i<pexStringTable.count) then begin
            Result := pexStringTable[i];
            exit;
        end;
        
        Result := '';
    end;
    
    procedure pexCleanUp();
    begin
        if(assigned(pexStringTable)) then begin
            pexStringTable.free();
            pexStringTable := nil;
        end;
        
        if(assigned(pexBr)) then begin
            pexBr.free();
            pexBr := nil;
        end;
        
        if(assigned(pexContainedObjects)) then begin
            pexContainedObjects.free();
            pexContainedObjects := nil;
        end;
        
        if(assigned(pexExtendedObjects)) then begin
            pexExtendedObjects.free();
            pexExtendedObjects := nil;
        end;
    end;
    
    procedure _pexReadHeader();
    var
        magic: cardinal;
    begin
        magic := pexBr.readUInt32(); // magic
        
        //pexBr.seek(4);//seek(4); // soBegin, soCurrent, soEnd
        pexMajorVersion := pexBr.readByte(); // just read = 1 byte
        pexMinorVersion := pexBr.readByte();
        pexGameId := pexBr.ReadUInt16();
        
        // it seems pascal doesn't support 64bit ints
        pexCompileTime1 := pexBr.ReadUInt32();
        pexCompileTime2 := pexBr.ReadUInt32();
        
        pexSourceName := _pexReadString();
        pexUserName := _pexReadString();
        pexMachineName := _pexReadString();
        
        //AddMessage(pexSourceName+' '+pexUserName+' '+pexMachineName);
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
    begin
        // Seems to be Groups? taken from the source code of scriptdump.exe from http://f4se.silverlock.org/
        cnt := pexBr.ReadUInt16();
        
            for i:=0 to cnt-1 do begin
            
            objName := pexBr.ReadUInt16();
            groupName := pexBr.ReadUInt16();
            groupDoc := pexBr.ReadUInt16();
            userFlags := pexBr.ReadUInt32();
            nameCount := pexBr.ReadUInt16();
            
            // the names seem to be more UInt16 indices
            
            skipBytes(nameCount * 2);
            {
            AddMessage('GROUP: '+pexGetStringTableEntry(objName)+' '+pexGetStringTableEntry(groupName));
            for j:=0 to nameCount-1 do begin
                AddMessage(pexGetStringTableEntry(pexBr.ReadUInt16()));
            end;
            }
        end;
    end;
    
    procedure _pexReadStructOrder();
    var
        cnt, i, j, objName, orderName, varCount, curVarName: integer;
    begin
        // Seems to be structs? taken from the source code of scriptdump.exe from http://f4se.silverlock.org/
        cnt := pexBr.ReadUInt16();
        
        for i:=0 to cnt-1 do begin 
            objName := pexBr.ReadUInt16();
            orderName := pexBr.ReadUInt16();
            varCount := pexBr.ReadUInt16();
            // AddMessage('Is this a struct? '+pexGetStringTableEntry(objName)+' '+pexGetStringTableEntry(orderName)+' varCount='+IntToStr(varCount));
            
            skipBytes(varCount * 2);
            {
            for j:=0 to varCount-1 do begin
                curVarName := pexBr.ReadUInt16();
                AddMessage('    Var? '+pexGetStringTableEntry(curVarName));
            end;
            }
        end;
    end;
    
    procedure _pexReadDebugInfo();
    var
        hasDebugInfo, modTime1, modTime2, numFuncs, i: integer;
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
        end;
    end;
    
    procedure _pexReadUserFlags();
    var
        userFlagCount, curNameIndex, curFlagIndex, i: integer;
    begin
        userFlagCount := pexBr.ReadUInt16();
        // just skip them for now
        skipBytes(userFlagCount*3);
        {
        //AddMessage('userFlagCount = '+IntToStr(userFlagCount));
        for i:=0 to userFlagCount-1 do begin
            curNameIndex := pexBr.ReadUInt16();
            curFlagIndex := pexBr.readByte();
            //AddMessage('Read Flag '+IntToStr(curNameIndex)+' -> '+IntToStr(curFlagIndex));
            //AddMessage('Read Flag '+pexStringTable[curNameIndex]+' -> '+IntToStr(curFlagIndex));
        end;
        }
    end;
    
    procedure _pexReadObject();
    var
        nameIndex, parentClassIndex, docstring, autoStateName, numVariables, constFlag: integer;
        size, userFlags, skipTo: cardinal;
    begin
        // getStringTableEntry
        nameIndex := pexBr.ReadUInt16();
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
            numVariables := pexBr.ReadUInt16();
            
            pexContainedObjects.add(pexGetStringTableEntry(nameIndex));
            pexExtendedObjects.add(pexGetStringTableEntry(parentClassIndex));


            //AddMessage('Found object '+pexGetStringTableEntry(nameIndex)+' extends '+pexGetStringTableEntry(parentClassIndex));
            
            // skip rest
            pexCurrentStream.position := skipTo;
            //pexCurrentStream.seek(skipTo, soFromBeginning);
        end; 
    end;
    
    procedure _pexReadObjects();
    var
        numObjects, i: integer;
    begin
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
            //AddMessage('About to read object');
            _pexReadObject();
        end;
    end;
    
    procedure _pexDoParse();
    begin
        _pexReadHeader();
        _pexReadStringTable();
        _pexReadDebugInfo();
        _pexReadPropertyGroups();
        _pexReadStructOrder();
        _pexReadUserFlags();
        _pexReadObjects();
    end;
    
    function pexReadFile(filename: string): boolean;
    var
        byteStream: TFileStream;
    begin
        Result := false;
        
        
        if(not FileExists(DataPath+filename)) then begin
            exit;
        end;
        
        
        pexContainedObjects := TStringList.create;
        pexExtendedObjects  := TStringList.create;

        try
            byteStream := TBytesStream.Create(ResourceOpenData('', filename));
            
            pexCurrentStream := byteStream;

            pexBr := TBinaryReader.Create(byteStream);

            _pexDoParse();
            Result := true;
        finally
            if(assigned(byteStream)) then begin
                byteStream.free();
            end;
        end;

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
    
end.