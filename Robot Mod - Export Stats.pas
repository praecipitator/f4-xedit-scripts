{
    Export robot stats to CSV. Run on OMODs.
}
unit ExportRobotMods;
    uses praUtil, robotModLib;
    
    var
        outData: TJsonObject;
        
        
    function IntToStrE(i: integer): string;
    begin
        if(i = 0) then begin
            Result := '';
            exit;
        end;
        
        Result := IntToStr(i);
    end;
        
        
    function stripDescription(desc: string): string;
    begin
        Result := regexReplace(desc, '\+[0-9]+.+$', '');
    end;
        
    function CreateSaveItemsFileDialog(title: string; filter: string = ''): TSaveDialog;
    var
        objFile: TSaveDialog;
    begin
        objFile := TSaveDialog.Create(nil);
        Result := nil;

        objFile.Title := title;


        if(filter <> '') then begin
            objFile.Filter := filter;
            objFile.FilterIndex := 1;
        end;
        Result := objFile;
    end;

    function ShowSaveItemsFileDialog(title: string): string;
    var
        objFile: TSaveDialog;
    begin
        objFile := CreateSaveItemsFileDialog(title, 'CSV Files|*.csv|All Files|*.*');
        Result := '';
        try
            if objFile.Execute then begin
                Result := objFile.FileName;
                
                if(not strEndsWith(Result, '.csv')) then begin
                    Result := Result + '.csv';
                end;
                
            end;
        finally
            objFile.free;
        end;
    end;
        
    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        outData := TJsonObject.create;
    end;
    
    procedure outputStats(omod: IInterface; strength, perception, endurance, charisma, intelligence, agility, luck: integer);
    var
        line, fileKey: string;
        obj: TJsonObject;
    begin
        {
        if (strength = 0) and (perception = 0) and (endurance = 0) and (charisma = 0) and (intelligence = 0) and (agility = 0) and (luck = 0) then begin
            exit;
        end;
        }
        
        fileKey := GetFileName(GetFile(omod));
        obj := outData.A[fileKey].addObject();
        
        obj.I['S'] := strength;
        obj.I['P'] := perception;
        obj.I['E'] := endurance;
        obj.I['C'] := charisma;
        obj.I['I'] := intelligence;
        obj.I['A'] := agility;
        obj.I['L'] := luck;
        obj.S['name'] := DisplayName(omod);
        obj.S['desc'] := stripDescription(GetElementEditValues(omod, 'DESC'));
        obj.S['edid'] := EditorID(omod);
        
        //line := EditorID(omod)+','+DisplayName(omod)+','+IntToStr(strength)+','+IntToStr(perception)+','+IntToStr(endurance)+','+IntToStr(charisma)+','+IntToStr(intelligence)+','+IntToStr(agility)+','+IntToStr(luck)+',';
       // AddMessage(line);
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        props, prop, ench, effects, effect, effBase: IInterface;
        mag, i, j, strength, perception, endurance, charisma, intelligence, agility, luck: integer;
        effectEdid: string;
    begin
        Result := 0;

        strength := 0;
        perception := 0;
        endurance := 0;
        charisma := 0;
        intelligence := 0;
        agility := 0;
        luck := 0;
        
        if(Signature(e) <> 'OMOD') then exit;
        
        if(GetElementEditValues(e, 'DATA\Form Type') <> 'Non-player character') then exit;


        // comment this out if you don't want those messages
        // AddMessage('Processing: ' + FullPath(e));
        props := ElementByPath(e, 'DATA\Properties');
        for i :=0 to ElementCount(props)-1 do begin
            prop := ElementByIndex(props, i);
            if(GetElementEditValues(prop, 'Property') = 'Enchantments') then begin
                ench := pathLinksTo(prop, 'Value 1 - FormID');
                effects := ElementByPath(ench, 'Effects');
           //     AddMessage('cnt'+IntToStr(ElementCount(effects)));
                for j:=0 to ElementCount(effects)-1 do begin
                    effect := ElementByIndex(effects, j);
                    effBase := pathLinksTo(effect, 'EFID');
                    mag := GetElementNativeValues(effect, 'EFIT\Magnitude');
                    //AddMessage('foo '+EditorID(effBase)+' '+IntToStr(mag));

                    effectEdid := EditorID(effBase);

                    if(effectEdid = 'praSS2Bot_Bot_FortifyStrength') then begin
                        strength := strength + mag;
                    end else if(effectEdid = 'praSS2Bot_Bot_FortifyPerception') then begin
                        perception := perception + mag;
                    end else if(effectEdid = 'praSS2Bot_Bot_FortifyEndurance') then begin
                        endurance := endurance + mag;
                    end else if(effectEdid = 'praSS2Bot_Bot_FortifyCharisma') then begin
                        charisma := charisma + mag;
                    end else if(effectEdid = 'praSS2Bot_Bot_FortifyIntelligence') then begin
                        intelligence := intelligence + mag;
                    end else if(effectEdid = 'praSS2Bot_Bot_FortifyAgility') then begin
                        agility := agility + mag;
                    end else if(effectEdid = 'praSS2Bot_Bot_FortifyLuck') then begin
                        luck := luck + mag;
                    end;
                end;

            end;            
        end;
        
        outputStats(e, strength, perception, endurance, charisma, intelligence, agility, luck);

        // processing code goes here

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        mainList: TStringList;
        curName, curHeader, curLine, targetFile: string;
        i, j: integer;
        curList: TJsonArray;
        curObj: TJsonObject;
    begin
        mainList := TStringList.create();
        targetFile := ShowSaveItemsFileDialog('Write CSV data to');
        if(FileExists(targetFile)) then begin
            mainList.LoadFromFile(targetFile);
            mainList.add(',,,,,,,,,');
        end;
        
        for i:=0 to outData.count-1 do begin
            curName := outData.names[i];
            curList := outData.A[curName];
            curHeader := '// '+curName+',,,,,,,,,';
            mainList.add(curHeader);
            //AddMessage(curHeader);
            curHeader := '// OMOD Editor ID,OMOD Name,S,P,E,C,I,A,L,Description';
            mainList.add(curHeader);
            //AddMessage(curHeader);
            for j:=0 to curList.count-1 do begin
                curObj := curList.O[j];
                curLine := curObj.S['edid']+',"'+curObj.S['name']+'",'+IntToStrE(curObj.I['S'])+','+IntToStrE(curObj.I['P'])+','+IntToStrE(curObj.I['E'])+','+IntToStrE(curObj.I['C'])+','+IntToStrE(curObj.I['I'])+','+IntToStrE(curObj.I['A'])+','+IntToStrE(curObj.I['L'])+',"'+curObj.S['desc']+'"';
                mainList.add(curLine);
                //AddMessage(curLine);
            end;
        end;
        
        AddMessage('Writing output file '+targetFile);
        mainList.saveToFile(targetFile);
        
        mainList.free();
        
        //AddMessage(outData.toString());
        Result := 0;
        
        outData.free();
    end;

end.