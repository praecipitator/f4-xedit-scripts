{
    Helper script to automatically give some robot armor pieces stats
}
unit setStatsToRobotArmor;

    uses praUtil, robotModLib;
    
    var
        nonArmorKws: TStringList;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    var
        leFile: IInterface;
    begin
        Result := 0;
        nonArmorKws := TStringList.create;
        nonArmorKws.add('ap_Bot_HomingMod');
        nonArmorKws.add('ap_DLC01Bot_Torso_Voice');
        nonArmorKws.add('ap_DLC01Bot_Paint');
        nonArmorKws.add('ap_Bot_BotLegs');
        nonArmorKws.add('ap_Bot_BotCore');
        nonArmorKws.add('ap_DLC01Bot_Torso_Misc');
        nonArmorKws.add('ap_Bot_HandRight');
        nonArmorKws.add('ap_Bot_HandLeft');
        nonArmorKws.add('ap_Bot_ArmRight');
        nonArmorKws.add('ap_Bot_ArmLeft');
        nonArmorKws.add('ap_Bot_SentryLeftShoulderMount');
        nonArmorKws.add('ap_Bot_SentryRightShoulderMount');
        nonArmorKws.add('ap_Bot_ModHands');
        nonArmorKws.add('ap_Bot_Head');
        nonArmorKws.add('ap_Bot_ArmsTypeA1');
        nonArmorKws.add('ap_Bot_ModSlotB');
        {
        [96] ap_Bot_ModSlotB "Center Slot" [KYWD:00047E28] // handy center eye
        [95] ap_Bot_ModArmsSlotA "Arm Mod Slot" [KYWD:00047E24] // thruster arm armor
        [56] ap_Bot_ModSlotA "Top Slot" [KYWD:00047E2C] // handy top armor
        }
        leFile := ShowFileSelectDialog('Select Target File');
        if(not assigned(leFile)) then begin
            Result := 1;
            exit;
        end;

        doInit(leFile);
    end;

    function hasCobj(e: IInterface): boolean;
    var
        i: integer;
        curRef, product: IInterface;
    begin
        Result := false;
        for i:=0 to ReferencedByCount(e)-1 do begin
            curRef := ReferencedByIndex(e, i);

            if(Signature(curRef) = 'COBJ') then begin
                product := PathLinksTo(curRef, 'CNAM');

                if(isSameForm(product, e)) then begin
                    Result := true;
                    exit;
                end;
            end else if(Signature(curRef) = 'FLST') then begin
                if(hasCobj(curRef)) then begin
                    Result := true;
                    exit;
                end;
            end;
        end;

    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        edid, kwEdid: string;
        attachPoint: IInterface;
    begin
        Result := 0;
        if(Signature(e) <> 'OMOD') then exit;
        
        edid := EditorID(e);
        
        if(not strStartsWithCI(edid, 'DLC01Bot_')) then exit;
        
        if(not hasCobj(e)) then exit;
        
        attachPoint := pathLinksTo(e, 'DATA\Attach Point');
        
        if(not assigned(attachPoint)) then exit;
        
    // DLC01Bot_Head_Assaultron_Armor_Construction10

        // comment this out if you don't want those messages
        
        //addMessage(GetElementEditValues(attachPoint, 'FULL'));
        
        kwEdid := EditorID(attachPoint);
        if(nonArmorKws.indexOf(EditorID(attachPoint)) > -1) then exit;
        
        // skip NULL and STANDARD
        if(pos('null', LowerCase(edid)) > 0) then exit;
        if(pos('standard', LowerCase(edid)) > 0) then exit;
        if(pos('factory', LowerCase(DisplayName(e))) > 0) then exit;
        
        // ugh disregard wasteland armors
        
        if (pos('construction', LowerCase(edid)) > 0) then begin
            AddMessage('Processing: ' + FullPath(e));
            if(pos('actuated', LowerCase(DisplayName(e))) > 0) then begin
                setStats(e, 0, 0, 1, 0, 0, 0, 0);
            end else if(pos('hydraulic', LowerCase(DisplayName(e))) > 0) then begin
                setStats(e, 1, 0, 0, 0, 0, 0, 0);
            end else if(pos('voltaic', LowerCase(DisplayName(e))) > 0) then begin
                setStats(e, 0, 0, 0, 0, 1, 0, 0);
            end else begin
                setStats(e, 0, 0, 1, 0, 0, 0, 0);
            end;
            
        end;

    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;
        nonArmorKws.free();
    end;

end.