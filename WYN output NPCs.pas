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

function getScriptCheckActor(actor: IInterface): IInterface;
var
    useScriptTemplate: boolean;
    templateActor: IInterface;
begin
    Result := actor;
    useScriptTemplate := GetElementEditValues(actor, 'ACBS\Use Template Actors\Script') = '1';
    if(useScriptTemplate) then begin
        // get template, recurse into
        templateActor := PathLinksTo(actor, 'TPTA\Script');
        if(assigned(templateActor)) then begin
            // AddMessage('got '+displayName(templateActor));
            Result := getScriptCheckActor(templateActor);
        end;
    end;

end;

function getFactionCheckActor(actor: IInterface): IInterface;
var
    useScriptTemplate: boolean;
    templateActor: IInterface;
begin
    Result := actor;
    useScriptTemplate := GetElementEditValues(actor, 'ACBS\Use Template Actors\Factions') = '1';
    if(useScriptTemplate) then begin
        // get template, recurse into
        templateActor := PathLinksTo(actor, 'TPTA\Factions');
        if(assigned(templateActor)) then begin
            // AddMessage('got '+displayName(templateActor));
            Result := getScriptCheckActor(templateActor);
        end;
    end;

end;

function checkSettlerScript(actor: IInterface): boolean;
var
    useScriptTemplate: boolean;
    templateActor: IInterface;
begin
    templateActor := getScriptCheckActor(actor);


    Result := assigned(templateActor(actor, 'workshopnpcscript'));
end;

function hasFaction(actor: IInterface; factionEdid: string): boolean;
var
    factions,curEntry, curFaction: IInterface;
    i: integer;
begin
    factions := ElementByPath(actor, 'Factions');
    for i:=0 to ElementCount(factions) do begin
        curEntry := ElementByIndex(factions, i);
        curFaction := PathLinksTo(curEntry, 'Faction');
        if(EditorID(curFaction) = factionEdid) then begin
            Result := true;
            exit;
        end;
    end;
    Result := false;
end;

// called for every record selected in xEdit
function Process(e: IInterface): integer;
var
    curFile, script, race, scriptCheckActor, factionsCheckActor: IInterface;
    npcName, outputStr: string;
    curFormId: cardinal;
begin
    Result := 0;





    if(Signature(e) <> 'NPC_') then exit;

    npcName := DisplayName(e);
    if(npcName = '') then exit;

    if(doBlacklist) then begin // for whitelist, just do it
        if(npcName = 'Settler') then exit;

        // condition is:
        {
            - race is in (HumanChildRace, HumanRace, GhoulChildRace, GhoulRace, PowerArmorRace)
            - has the script
            - is not hostile to the player (can't check here)
            - does NOT have companionactorscript
            - is NOT in DomesticAnimalFaction
            - is not dead (can't check here)
            - is in faction WorkshopNPCFaction
        }
        scriptCheckActor := getScriptCheckActor(e);
        //if(not checkSettlerScript(e)) then exit;

        // Add extended script check
        script := getScript(scriptCheckActor, 'workshopnpcscript');

        if(not assigned(script)) then exit;
        //AddMessage('no workshopnpcscript');

        script := getScript(scriptCheckActor, 'companionactorscript');
        if(assigned(script)) then exit;
        //AddMessage('has companionactorscript');

        race := pathLinksTo(e, 'RNAM');

        if(validRaces.indexOf(EditorID(race)) < 0) then exit;

        factionsCheckActor := getFactionCheckActor(e);
        if(hasFaction(factionsCheckActor, 'DomesticAnimalFaction')) then exit;
        if(not hasFaction(factionsCheckActor, 'WorkshopNPCFaction')) then exit;
    end;



    e := MasterOrSelf(e);
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