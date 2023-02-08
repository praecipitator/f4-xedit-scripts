{
    Run on NPCs, it will generate addModEntryToFormlist commands
}
unit userscript;

uses praUtil;

var
    doBlacklist: boolean;
    flstName, modeName: string;
    validRaces: TStringList;

// Called before processing
// You can remove it if script doesn't require initialization code
function Initialize: integer;
begin
    Result := 0;
    doBlacklist := true;

    if(doBlacklist) then begin
        flstName := 'formBlackList';
        modeName := 'blacklist';
    end else begin
        flstName := 'actorWhiteList';
        modeName := 'whitelist';
    end;

    validRaces := TStringList.create;
    validRaces.add('HumanRace');
    validRaces.add('HumanChildRace');
    validRaces.add('GhoulRace');
    validRaces.add('GhoulChildRace');
end;

// called for every record selected in xEdit
function Process(e: IInterface): integer;
var
    curFile, script, race: IInterface;
    npcName, outputStr: string;
    curFormId: cardinal;
begin
    Result := 0;

    if(Signature(e) <> 'NPC_') then exit;

    npcName := DisplayName(e);
    if(npcName = '') then exit;

    script := getScript(e, 'workshopnpcscript');
    if(not assigned(script)) then exit;

    race := pathLinksTo(e, 'RNAM');
    
    if(validRaces.indexOf(EditorID(race)) < 0) then exit;

    curFile := GetFile(e);

    curFormId := FormID(e);

    if(IsFileLight(curFile)) then begin
        curFormId := $FFF and curFormId;
    end else begin
        curFormId := $FFFFFF and curFormId;
    end;


    outputStr := 'addModEntryToFormlist(0x'+IntToHex(curFormId, 8)+', "' + GetFileName(curFile) + '", '+flstName+', "'+modeName+'")';

    outputStr := outputStr + ' ; ' + EditorID(e)+' "'+npcName+'"';// + EditorID(race);

    AddMessage(outputStr);
    // addModEntryToFormlist(0x0005D858, "RecruitableSettlersFH.esp", formBlackList, "blacklist")
    // addModEntryToFormlist(0x0000BE4C, "DLCNukaWorld.esm", actorWhiteList, "whitelist")

end;

// Called after processing
// You can remove it if script doesn't require finalization code
function Finalize: integer;
begin
    Result := 0;
    validRaces.free();
end;

end.