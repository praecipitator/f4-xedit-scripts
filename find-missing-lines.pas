{
    Run on quests, it will try to check if all the lines have the proper fuz files
}
unit userscript;
    uses praUtil;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
    end;
    
    function hasFileAnywhere(path: string): boolean;
    begin
        Result := ResourceExists(path);
    end;

    procedure processInfo(info, vType: IInterface; pathBase: string);
    var
        i: integer;
        infoFormID: cardinal;
        responses, resp: IInterface;
        nameBase, text, path: string;
    begin
        nameBase := GetElementEditValues(info, 'IOVR');
        if(nameBase = '') then begin
            infoFormID := FormID(info) and $00FFFFFF;
            nameBase :=  IntToHex(infoFormID, 8);
        end;

        responses := ElementByPath(info, 'Responses');
        // dumpElem(info);
        for i:=0 to ElementCount(responses)-1 do begin
            resp := ElementByIndex(responses, i);

            text := GetElementEditValues(resp, 'NAM1');
            
            path := pathBase + EditorID(vType)+'\'+nameBase+'_'+IntToStr(i+1)+'.fuz';
            
            if(not hasFileAnywhere(path)) then begin
                AddMessage('"'+text+'";"'+path+'"');
            end;
        end;
    end;

    procedure processDial(dial, vType: IInterface; pathBase: string);
    var
        i: integer;
        infoGroup, info: IInterface;
    begin
        infoGroup := ChildGroup(dial);
        for i:=0 to ElementCount(infoGroup)-1 do begin
            info := ElementByIndex(infoGroup, i);
            processInfo(info, vType, pathBase);
        end;

    end;

    procedure processQuest(quest: IInterface);
    var
        i: integer;
        firstCond, conds, dial, vType, CTDA: IInterface;
        pathBase: string;
    begin
        conds := ElementByPath(quest, 'Quest Dialogue Conditions');
        
        pathBase := 'Sound\Voice\'+GetFileName(GetFile(quest))+'\';

        if(assigned(conds)) then begin
            if(ElementCount(conds) > 0) then begin
                firstCond := ElementByIndex(conds, 0);

                CTDA := ElementByPath(firstCond, 'CTDA');
                vType := PathLinksTo(ElementByIndex(CTDA, 0), 'Voice Type');
            end;
        end;


        for i := 0 to ReferencedByCount(quest)-1 do begin
            dial := ReferencedByIndex(quest, i);
            if Signature(dial) = 'DIAL' then begin
                processDial(dial, vType, pathBase);
            end;
        end;
    end;


    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        // comment this out if you don't want those messages

        if(Signature(e) = 'QUST') then begin
            AddMessage('Processing: ' + FullPath(e));
            processQuest(e);
        end;

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
    end;

end.