{
    Run on dialogues (or the quest containing them). It should copy files to Data/Sound/Voice/<yourMod.esp>/<voiceType>/
}
unit userscript;

uses praUtil;

var
    srcFile: IInterface;
    srcQuest: IInterface;
    voiceNames: TStringList;

// Called before processing
// You can remove it if script doesn't require initialization code
function Initialize: integer;
begin
    Result := 0;

    srcQuest := FindObjectByEdid('DLC06WorkshopParent');
    srcFile := GetFile(srcQuest);
    if(not assigned(srcQuest)) then begin
        Result := 1;
    end;

    voiceNames := TStringList.create();

    voiceNames.add('FemaleBoston');
    voiceNames.add('FemaleEvenToned');
    voiceNames.add('FemaleRough');

    voiceNames.add('MaleBoston');
    voiceNames.add('MaleEvenToned');
    voiceNames.add('MaleRough');
end;

function getInfoText(info: IInterface): string;
var
    curResponses, curRsp: IInterface;
begin
    Result := '';
    curResponses := ElementByPath(info, 'Responses');
    if(ElementCount(curResponses) <> 1) then exit;

    curRsp := ElementByIndex(curResponses, 0);

    Result := GetElementEditValues(curRsp, 'NAM1');
end;

function findMatchingText(text: string): IInterface;
var
    i, j: integer;
    curDial, curInfo, questGroup, dialGroup: IInterface;
    curSig, curText: string;
begin
    Result := nil;
    questGroup := ChildGroup(srcQuest);
    for i:=0 to ElementCount(questGroup)-1 do begin
        curDial := ElementByIndex(questGroup, i);
        curSig := Signature(curDial);
        if(curSig <> 'DIAL') then continue;

        dialGroup := ChildGroup(curDial);
        for j:=0 to ElementCount(dialGroup)-1 do begin
            curInfo := ElementByIndex(dialGroup, j);
            curText := getInfoText(curInfo);
            if(curText = '') then continue;

            if(text = curText) then begin
                // AddMessage('Found "'+curText+'"');
                Result := curInfo;
                exit;
            end;
        end;

    end;
end;

// called for every record selected in xEdit
function Process(e: IInterface): integer;
var
    curText, origName, targetName: string;
    curFormId, myFormId: cardinal;
    otherInfo: IInterface;
    i: integer;
begin
    Result := 0;

    if(Signature(e) <> 'INFO') then begin
        exit;
    end;

    curText := getInfoText(e);
    if(curText = '') then begin
        exit;
    end;

    // comment this out if you don't want those messages
    otherInfo := findMatchingText(curText);
    if(assigned(otherInfo)) then begin
        curFormId := getLocalFormId(srcFile, FormId(otherInfo));
        myFormId := getLocalFormId(GetFile(e), FormId(e));

        for i:=0 to voiceNames.count-1 do begin
            origName := 'Sound\Voice\'+GetFileName(GetFile(otherInfo))+'\' + voiceNames[i] + '\' + IntToHex(curFormId, 8) + '_1.fuz';
            
            targetName := DataPath+'Sound\Voice\'+GetFileName(GetFile(e))+'\' + voiceNames[i] + '\' + IntToHex(myFormId, 8) + '_1.fuz';
            
            AddMessage(origName+' -> '+targetName);
            
            ResourceCopy('DLCworkshop03 - Voices_en.ba2', origName, targetName);
        end;


        // curFormId := $01000000 or curFormId;

        //curName := IntToHex(curFormId, 8);
        // addMessage(curName);

        // SetElementEditValues(e, 'IOVR', curName);
    end;
    // processing code goes here

end;

// Called after processing
// You can remove it if script doesn't require finalization code
function Finalize: integer;
begin
    Result := 0;
    voiceNames.free();
end;

end.