{
    Generates meshes based on splines. Can optionally generate new statics and references at the same positions as the original splines.
    Run on one or several references of splines.

    The script will attempt to set up textures and materials according to the spline's TXST.
    If no TXST is set up, it will instead use the spline color as vertex colors, and use a simple texture. That's the best I can do, you have to use NifSkope to improve it.

    For best results, I recommend using splines with textures, or, better, a material.

    Options explained:
    - "Only process splines on this layer":
        A reference will only be processed, if it's on the given layer. If left empty, all references will be processed.
        You can use "default" to filter by the default layer specifically.

    - "Output Path (relative to Data)":
        Where to write the generated nif files. As the name suggests, this must be relative to the Fo4 Data folder.
        If the folder doesn't exist, the script will attempt to create it.

    - "Prefix for separate nif files":
        This prefix will be used for the filenames of nif files generated from individual references.
        The script will append the original reference's FormID to this.
        Only relevant if "Combine all splines into one single mesh" is NOT checked.

    - "Prefix for combined nif files":
        This prefix will be used for the filename of the single combined nif file generated from all selected references.
        The script will append a hash generated from all of the references' FormID to this.
        Only relevant if "Combine all splines into one single mesh" is checked.

    - "Default texture for texture-less splines":
        The script will use this texture for texture-less splines.
        A BSLightingShaderProperty absolutely requires a texture, or it will be pink.
        The default texture isn't perfect, if you have something better, you can put it in here.
        Default: Shared\FlatWhite01_d.dds

    - "Create statics and references using the meshes":
        If enabled, the script will create new static forms and new references as a replacement for the original splines.
        These references will be placed at the same locations as the original splines.
        The original splines will NOT be touched.

    - "Prefix for new Statics":
        This prefix will be used for the Editor IDs of the newly-generated static forms.
        The FormID of the individual references, or a hash calculcated of all the contained references will then be appended to the prefix.

    - "Layer for new references":
        If not empty, new references will be put on this layer.
        Otherwise they will stay on the default layer.


    - "Combine all splines into one single mesh":
        If checked, the script will generate one single nif, containing all the selected splines in it.

    - "Origin of combined mesh":
        Cell coordinates where the combined nif's origin should be located.
        Effectively, this will be simply subtracted from all the individual TriShapes in the combined nif.


}
unit SplineToMesh;
    uses praUtil;
    uses CobbLibrary;


    const
        // Number of segments created along the way. No clue how many Bethesda makes, but it seems to work well enough with 22.
        // You can increase it to get a smoother curve.
        NUM_SEGMENTS = 22;

        // Factor which needs to be multiplied onto the distance between the points to get how far downards the spline extends. Found empirically.
        TODD_FACTOR = 1.4;

        // Half of the crosssection of a default spline with thickness 1. Found empirically.
        CROSSECTION_HALF = 5;

        // name of the config file
        CONFIG_FILE_NAME = ScriptsPath + 'Spline To Mesh.cfg';

        DEFAULT_WINDOW_WIDTH = 450;
        DEFAULT_WINDOW_HEIGHT = 430;

    var
        currentRopeDistance: float;

        currentNif: TwbNifFile;

        cfgMeshBasePath: string;
        cfgCombineMesh: boolean;
        cfgOriginX: float;
        cfgOriginY: float;
        cfgOriginZ: float;

        cfgCreateNewRefs: boolean;
        cfgNewRefsLayerName: string;
        cfgSplineLayer: string;
        cfgDefaultShaderTexture: string;
        cfgMeshPrefix: string;
        cfgMeshPrefixCombined: string;
        cfgFormPrefix: string;

        cfgWindowWidth: float;
        cfgWindowHeight: float;

        currentNameBaseString: string;
        currentNameComponents: TStringList;

        currentRefCell: IInterface;

    //////////////////////////// CONFIG STUFF BEGIN ///////////////////////////
    procedure loadConfig();
    var
        i, j, breakPos: integer;
        curLine, curKey, curVal: string;
        lines : TStringList;
    begin
        // default
        //cfgMeshBasePath := DataPath + 'Meshes\SplineToMesh\';
        cfgMeshBasePath := 'Meshes\SplineToMesh\';
        cfgCombineMesh := false;
        cfgOriginX := 0;
        cfgOriginY := 0;
        cfgOriginZ := 0;
        cfgCreateNewRefs := true;
        cfgNewRefsLayerName := '';
        cfgSplineLayer := '';
        cfgDefaultShaderTexture := 'Shared\FlatWhite01_d.dds';
        cfgMeshPrefix := 'spline_';
        cfgMeshPrefixCombined := 'splines_';
        cfgFormPrefix := 'staticSpline_';
        cfgWindowWidth := DEFAULT_WINDOW_WIDTH;
        cfgWindowHeight:= DEFAULT_WINDOW_HEIGHT;

        if(not FileExists(CONFIG_FILE_NAME)) then begin
            exit;
        end;
        lines := TStringList.create;
        lines.LoadFromFile(CONFIG_FILE_NAME);

        //
        for i:=0 to lines.count-1 do begin
            curLine := lines[i];
            breakPos := -1;

            for j:=1 to length(curLine) do begin
                if(curLine[j] = '=') then begin
                    breakPos := j;
                    break;
                end;
            end;

            if breakPos <> -1 then begin
                curKey := trim(copy(curLine, 0, breakPos-1));
                curVal := trim(copy(curLine, breakPos+1, length(curLine)));

                if(curKey = 'cfgMeshBasePath') then begin
                    cfgMeshBasePath := curVal;
                end else if(curKey = 'cfgCombineMesh') then begin
                    cfgCombineMesh := StrToBool(curVal);
                end else if(curKey = 'cfgOriginX') then begin
                    cfgOriginX := StrToFloat(curVal);
                end else if(curKey = 'cfgOriginY') then begin
                    cfgOriginY := StrToFloat(curVal);
                end else if(curKey = 'cfgWindowWidth') then begin
                    cfgWindowWidth := StrToFloat(curVal);
                end else if(curKey = 'cfgWindowHeight') then begin
                    cfgWindowHeight := StrToFloat(curVal);
                end else if(curKey = 'cfgOriginZ') then begin
                    cfgOriginZ := StrToFloat(curVal);
                end else if(curKey = 'cfgCreateNewRefs') then begin
                    cfgCreateNewRefs := StrToBool(curVal);
                end else if(curKey = 'cfgDefaultShaderTexture') then begin
                    cfgDefaultShaderTexture := curVal;
                end else if(curKey = 'cfgMeshPrefixCombined') then begin
                    cfgMeshPrefixCombined := curVal;
                end else if(curKey = 'cfgMeshPrefix') then begin
                    cfgMeshPrefix := curVal;
                end else if(curKey = 'cfgFormPrefix') then begin
                    cfgFormPrefix := curVal;
                end else if(curKey = 'cfgSplineLayer') then begin
                    cfgSplineLayer := curVal;
                end else if(curKey = 'cfgNewRefsLayerName') then begin
                    cfgNewRefsLayerName := curVal;
                end;
            end;
        end;

        lines.free();
    end;

    procedure saveConfig();
    var
        lines : TStringList;
    begin
        lines := TStringList.create;

        {
        cfgMeshBasePath := DataPath + 'Meshes\SplineToMesh\';
        cfgCombineMesh := false;
        cfgOriginX := 0;
        cfgOriginY := 0;
        cfgOriginZ := 0;
        cfgCreateNewRefs := true;
        cfgNewRefsLayerName := '';
        cfgDefaultShaderTexture := 'Shared\FlatWhite01_d.dds';
        cfgMeshPrefix := 'spline_';
        cfgFormPrefix := 'staticSpline_';


        }


        lines.add('cfgMeshBasePath='+cfgMeshBasePath);
        lines.add('cfgCombineMesh='+BoolToStr(cfgCombineMesh));
        lines.add('cfgOriginX='+FloatToStr(cfgOriginX));
        lines.add('cfgOriginY='+FloatToStr(cfgOriginY));
        lines.add('cfgOriginZ='+FloatToStr(cfgOriginZ));
        lines.add('cfgWindowWidth='+FloatToStr(cfgWindowWidth));
        lines.add('cfgWindowHeight='+FloatToStr(cfgWindowHeight));
        lines.add('cfgCreateNewRefs='+BoolToStr(cfgCreateNewRefs));
        lines.add('cfgNewRefsLayerName='+cfgNewRefsLayerName);
        lines.add('cfgFormPrefix='+cfgFormPrefix);
        lines.add('cfgSplineLayer='+cfgSplineLayer);
        lines.add('cfgDefaultShaderTexture='+cfgDefaultShaderTexture);
        lines.add('cfgMeshPrefix='+cfgMeshPrefix);
        lines.add('cfgMeshPrefixCombined='+cfgMeshPrefixCombined);

        lines.saveToFile(CONFIG_FILE_NAME);
        lines.free();
    end;


    //////////////////////////// NIF STUFF BEGIN //////////////////////////////


    function createNifBase(): TJsonObject;
    var
        niHeader, exportInfo, rootNode, transform, bsxFlags, triShape, boundingSphere, VertexDesc: TJsonObject;
        BSEffectShaderProperty, bsEffectCtrler, interpolator, floatData, key: TJsonObject;
        alphaProp, vertex: TJsonObject;
        blockTypes: TJsonArray;
    begin
        Result := TJsonObject.create;
        // header
        niHeader := Result.O['NiHeader'];
        niHeader.S['Magic'] := 'Gamebryo File Format, Version 20.2.0.7';
        niHeader.S['Version'] := '20.2.0.7';
        niHeader.S['Endian Type'] := 'ENDIAN_LITTLE';
        niHeader.S['User Version'] := '12';
        niHeader.S['Num Blocks'] := '2';
        niHeader.S['User Version 2'] := '130';

        exportInfo := niHeader.O['Export Info'];
        exportInfo.S['Author'] := 'xEdit, pra';
        exportInfo.S['Process Script'] := 'Spline To Mesh.pas';
        exportInfo.S['Export Script'] := 'Spline To Mesh.pas';

        niHeader.S['Max Filepath'] := '';// or maybe put something here
        blockTypes := niHeader.A['Block Types'];
        blockTypes.Add('NiNode');
        blockTypes.Add('BSXFlags');

        blockTypes := niHeader.A['Block Type Index'];
        blockTypes.Add('NiNode');
        blockTypes.Add('BSXFlags');

        // root node
        rootNode := Result.O['0 NiNode'];
        rootNode.S['Name'] := 'RootNode';
        rootNode.A['Extra Data List'].Add('1 BSXFlags "BSX"');
        rootNode.S['Controller'] := 'None';
        rootNode.S['Flags'] := '14';
        rootNode.S['Collision Object'] := 'None';

        transform := rootNode.O['Transform'];
        transform.S['Translation'] := '0.000000 0.000000 0.000000';
        transform.S['Rotation'] := '0.000000 0.000000 0.000000';
        transform.S['Scale'] := '1.000000';

        // BSX flags
        bsxFlags := Result.O['1 BSXFlags'];
        bsxFlags.S['Name'] := 'BSX';

        // finally the footer
        Result.O['NiFooter'].A['Roots'].Add('0 NiNode "RootNode"');
    end;


    function formatFloatForNif(f: float): string;
    begin
        Result := FormatFloat('0.000000', f);
    end;

    function createHexColor(r, g, b, a: integer): string;
    begin
        Result := '#' + IntToHex(r, 2)+ IntToHex(g, 2)+ IntToHex(b, 2)+ IntToHex(a, 2);
    end;

    function writeVertex(x, y, z: float; vertData: TwbNifBlock; r, g, b: integer; u, v: float): TwbNifBlock;
    var
        newVertex: TwbNifBlock;
    begin
        Result := vertData.Add();
        Result.Elements['Vertex'].NativeValues['X'] := x;
        Result.Elements['Vertex'].NativeValues['Y'] := y;
        Result.Elements['Vertex'].NativeValues['Z'] := z;

        Result.EditValues['Vertex Colors'] := createHexColor(r, g, b, 255);


        Result.EditValues['UV'] := formatFloatForNif(u)+' '+formatFloatForNif(v);


    end;

    function writeTriangle(i1, i2, i3: integer; triData: TwbNifBlock): TwbNifBlock;
    begin
        Result := triData.Add();
        Result.EditValue := IntToStr(i1)+' '+IntToStr(i2)+' '+IntToStr(i3);
    end;


    procedure quaternionNormalizeInPlace(var q: TJsonObject);
    var
        len: float;
    begin
        len := sqrt( sqr(q.F['w']) + sqr(q.F['x']) + sqr(q.F['y']) + sqr(q.F['z']) );
        q.F['w'] := q.F['w'] / len;
        q.F['x'] := q.F['x'] / len;
        q.F['y'] := q.F['y'] / len;
        q.F['z'] := q.F['z'] / len;

    end;

    function getRotationFromDeltaVector(deltaVector: TJsonObject): TJsonObject;
    var
        baseVector, crossProd: TJsonObject;
        quaternion: TJsonObject;
    begin
        baseVector := newVector(0,0,-1);
        // https://stackoverflow.com/questions/1171849/finding-quaternion-representing-the-rotation-from-one-vector-to-another
        crossProd := VectorCrossProduct(baseVector, deltaVector);
        quaternion := newQuaternion(
            VectorDotProduct(baseVector, deltaVector) + sqrt(sqr(VectorLength(baseVector)) * sqr(VectorLength(deltaVector))),
            crossProd.F['x'],
            crossProd.F['y'],
            crossProd.F['z']
        );

        quaternionNormalizeInPlace(quaternion);


        Result := QuaternionToEuler(quaternion);

        quaternion.free();
        crossProd.free();
        baseVector.free();
    end;

    procedure writePointToNif(i: integer; pointChain: TJsonArray; vertData, triData: TwbNifBlock; thickness: float; colorR, colorG, colorB, numSides: integer; numTiles :float; relativeToLength: boolean; ropeLength: float);
    var
        curPoint, nextPoint, prevPoint, deltaVector: TJsonObject;
        parentPos, parentRot, offsetPos, offsetRot: TJsonObject;
        relData: TJsonObject;
        distanceValue: float;

        rotAngle, curAngle, j, curX, curY: float;
        firstVertIndex, firstVertOtherIndex, secondVertIndex, secondVertOtherIndex: integer;
        vertexU: float;
        vertexV: float;
        distToPrev: float;
    begin
        distanceValue := CROSSECTION_HALF * thickness / 10; // distance from the center

        curPoint := pointChain.O[i];
        if(i=0) then begin
            // at the start
            nextPoint := pointChain.O[i+1];

            deltaVector := VectorSubtract(nextPoint, curPoint);
            distToPrev := 0;
            currentRopeDistance := 0;
        end else if(i=pointChain.count-1) then begin
            // at the end
            prevPoint := pointChain.O[i-1];

            distToPrev := getPointDistance(prevPoint, curPoint);
            currentRopeDistance := currentRopeDistance + distToPrev;

            deltaVector := VectorSubtract(curPoint, prevPoint);
        end else begin
            // in the middle
            nextPoint := pointChain.O[i+1];
            prevPoint := pointChain.O[i-1];

            distToPrev := getPointDistance(prevPoint, curPoint);
            currentRopeDistance := currentRopeDistance + distToPrev;

            deltaVector := VectorSubtract(nextPoint, prevPoint);
        end;

        parentPos := newVector(curPoint.F['x'], curPoint.F['y'], curPoint.F['z']);

        parentRot := getRotationFromDeltaVector(deltaVector);

        offsetRot := newVector(0, 0, 0);


        {
            old:
                0--1--------6--7
               /    \      /    \
              5  x   2----11-----8
               \    /      \    /
                4--3-------10--9

            new:

                6 0-1-------13 7-8
               /     \      /     \
              5   x   2----12--x---9
               \     /      \     /
                4---3-------11---10

            6 and 0, 13 and 7 have the same coords
            0 and 7 have V = 0, 7 and 13 have V = 1

            The 6-0 and 13-7 seam is necessary for UV reasons.
        }

        // numSides := 6;
        rotAngle := 360 / numSides;

        for j:=0 to numSides do begin
            // now we start off with a vector like (distanceValue, 0, 0), and need to rotate it on the Z axis by i*rotAngle
            if (j = numSides) or (j = 0) then begin
                curAngle := 0;
            end else begin
                curAngle := rotAngle * j * -1;
            end;
            // AddMessage('curAngle = '+FloatToStr(curAngle));
            curX := distanceValue * cosDeg(curAngle);
            curY := distanceValue * sinDeg(curAngle);

            offsetPos := newVector(curX, curY, 0);
            relData := GetCoordinatesRelativeToBase(parentPos, parentRot, offsetPos, offsetRot);
            // AddMessage('relData = '+relData.toString());

            // U foward, V is upward?
            vertexV := j/(numSides);

            // now how? This might be the point where relativeToLength might be relevant
            // relativeLength might mean: if not checked, the start of the spline is 0, the end is 1
            // NO! it means, start is 0, but end is dependent on the numTiles
            // distToPrev
            if(i = 0) then begin
                vertexU := 0;
            end else begin
                if(not relativeToLength) then begin
                    vertexU := (currentRopeDistance / ropeLength) * numTiles;
                    // AddMessage('currentRopeDistance='+FloatToSTr(currentRopeDistance)+', ropeLength='+FloatToStr(ropeLength)+', numTiles='+FloatToStr(numTiles));
                end else begin
                    vertexU := (currentRopeDistance) * numTiles * 2;
                end;
            end;


            writeVertex(relData.O['pos'].F['x'], relData.O['pos'].F['y'], relData.O['pos'].F['z'], vertData, colorR, colorG, colorB, vertexU, vertexV);

            relData.free();
            offsetPos.free();
        end;

        parentPos.free();
        parentRot.free();
        offsetRot.free();
        deltaVector.free();


        // try making tris
        if(i>0) then begin
            // ok, we added numSides verticles.
            // the last vert is vertData.count-1;

            for i:=0 to numSides-1 do begin
                firstVertIndex := vertData.count - 2*(numSides+1) + i;
                firstVertOtherIndex := firstVertIndex + numSides+1;

                secondVertIndex := firstVertIndex + 1;

                secondVertOtherIndex := secondVertIndex + numSides + 1;
                // now make the 2 tris out of these
                {
                firstVertIndex-----firstVertOtherIndex
                    |            /           |
                    |           /            |
                secondVertIndex-----secondVertOtherIndex
                }
                writeTriangle(secondVertIndex, firstVertOtherIndex, firstVertIndex, triData);
                writeTriangle(secondVertIndex, secondVertOtherIndex, firstVertOtherIndex, triData);
            end;
        end;
    end;

    procedure ClearTriangleData(Triangles: TwbNifBlock);
    var
        entry: TwbNifBlock;
    begin
        while Triangles.count > 0 do begin
            entry := Triangles[0];
            entry.remove();
        end;
    end;

    procedure ClearVertexData(vertData: TwbNifBlock);
    var
        vert: TwbNifBlock;
    begin

        while vertData.count > 0 do begin
            vert := vertData[0];
            vert.remove();
        end;
    end;

    function createBSLightingShaderProperty(nif: TwbNifFile; materialName: string): TwbNifBlock;
    var
        newObj: TJsonObject;

    begin
        Result := nif.InsertBlock(1 , 'BSLightingShaderProperty');
        Result.EditValues['Name'] := materialName;
    end;

    procedure setShaderTextures(nifTextureSet: TwbNifBlock; textureSet: IInterface);
    var
        texturesBlock, texturesRoot: IInterface;
        texturesSubEntry: TwbNifBlock;
        i: integer;
    begin
        texturesSubEntry := nifTextureSet.Elements['Textures'];
        for i:=0 to 7 do begin
            texturesSubEntry.Add();
        end;



        if assigned(textureSet) then begin
            texturesRoot := ElementByPath(textureSet, 'Textures (RGB/A)');

            texturesSubEntry[0].EditValue := GetElementEditValues(texturesRoot, 'TX00 - Diffuse');
            texturesSubEntry[1].EditValue := GetElementEditValues(texturesRoot, 'TX01 - Normal/Gloss');
            texturesSubEntry[2].EditValue := GetElementEditValues(texturesRoot, 'TX03 - Glow');
            texturesSubEntry[3].EditValue := GetElementEditValues(texturesRoot, 'TX04 - Height');
            texturesSubEntry[4].EditValue := GetElementEditValues(texturesRoot, 'TX05 - Environment');
            texturesSubEntry[5].EditValue := GetElementEditValues(texturesRoot, 'TX02 - Wrinkles');
            texturesSubEntry[6].EditValue := GetElementEditValues(texturesRoot, 'TX06 - Multilayer');
            texturesSubEntry[7].EditValue := GetElementEditValues(texturesRoot, 'TX07 - Smooth Spec');
        end else begin
            texturesSubEntry[0].EditValue := cfgDefaultShaderTexture;
        end;

    end;

    function getTotalRopeLength(pointChain: TJsonArray): float;
    var
        i: integer;
        curPoint, prevPoint: TJsonObject;
    begin
        Result := 0;
        for i:=1 to pointChain.count-1 do begin
            curPoint := pointChain.O[i];
            prevPoint := pointChain.O[i-1];

            Result := Result + getPointDistance(prevPoint, curPoint);
        end;
    end;

    function startNif(): TwbNifFile;
    var
        nifJson: TJsonObject;
    begin
        nifJson := createNifBase();

        Result := TwbNifFile.Create;

        Result.FromJson(nifJson.toString());
        nifJson.free();
    end;

    function appendSplineTrishape(nif : TwbNifFile; pointChain: TJsonArray; nameBase: string; ref: IInterface): TwbNifBlock;
    var
        thickness, baseForm: IInterface;


        rootNode, triShapeBlock, shaderBlock, vertexData, Triangles: TwbNifBlock;
        nifTextureSet: TwbNifBlock;

        j: integer;
        materialName: string;

        textureSet: IInterface;
        numSides, colorR, colorG, colorB: integer;
        numTiles: float;
        relativeToLength: boolean;

        splineLength: float;
    begin
        thickness := getElementNativeValues(ref, 'XBSD\Thickness');
        //AddMessage('thickness='+FloatToSTr(thickness));
        baseForm := pathLinksTo(ref, 'NAME');

        splineLength := getTotalRopeLength(pointChain);

        textureSet := pathLinksTo(baseForm, 'TNAM - Texture');
        colorR := getElementNativeValues(baseForm, 'DNAM\Default Color\Red');
        colorG := getElementNativeValues(baseForm, 'DNAM\Default Color\Green');
        colorB := getElementNativeValues(baseForm, 'DNAM\Default Color\Blue');

        numSides := getElementNativeValues(baseForm, 'DNAM\Default Number of Slices');
        numTiles := getElementNativeValues(baseForm, 'DNAM\Default Number of Tiles');
        relativeToLength := getElementNativeValues(baseForm, 'DNAM\Default Number of Tiles - Relative to Length');



        rootNode := nif.BlockByName('RootNode');

        triShapeBlock := nif.InsertBlock(1, 'BSTriShape');
        triShapeBlock.EditValues['Name'] := 'Spline';
        triShapeBlock.NativeValues['VertexDesc\VF\VF_COLORS'] := 1;
        triShapeBlock.NativeValues['VertexDesc\VF\VF_UV'] := 1;
        triShapeBlock.NativeValues['VertexDesc\VF\VF_VERTEX'] := 1;
        triShapeBlock.NativeValues['VertexDesc\VF\VF_NORMAL'] := 1;
        triShapeBlock.NativeValues['VertexDesc\VF\VF_TANGENT'] := 1;


        rootNode.Elements['Children'].Add.NativeValue := triShapeBlock.Index;

        vertexData := triShapeBlock.Elements['Vertex Data'];
        Triangles := triShapeBlock.Elements['Triangles'];


        // material
        materialName := '';
        if(assigned(textureSet)) then begin
            materialName := GetElementEditValues(textureSet, 'MNAM');
            // shaderBlock.EditValues['Name'] := materialName;
            colorR := '255';
            colorG := '255';
            colorB := '255';
        end;

        shaderBlock := createBSLightingShaderProperty(nif, materialName);

        triShapeBlock.Elements['Shader Property'].NativeValue := shaderBlock.Index;



        // texture set
        nifTextureSet := nif.InsertBlock(1, 'BSShaderTextureSet');
        shaderBlock.Elements['Texture Set'].NativeValue := nifTextureSet.Index;
        setShaderTextures(nifTextureSet, textureSet);


        // direct nif object code

        ClearVertexData(vertexData);
        ClearTriangleData(Triangles);

        currentRopeDistance := 0;



        for j:=0 to pointChain.count-1 do begin
            writePointToNif(j, pointChain, vertexData, Triangles, thickness, colorR, colorG, colorB, numSides, numTiles, relativeToLength, splineLength);
        end;

        triShapeBlock.EditValues['Num Triangles'] := Triangles.count;
        triShapeBlock.EditValues['Num Vertices'] := vertexData.count;

        // https://github.com/TES5Edit/TES5Edit/blob/6a6b9ca787ec1e89f2d634a954cc5b87a7a3630e/Core/wbDataFormatNif.pas#L144
        // UpdateBounds, UpdateNormals, UpdateTangents might work. might be functions of TriShape
        triShapeBlock.UpdateBounds();

        Result := triShapeBlock;


    end;

    function finalizeNif(nif: TwbNifFile; nameBase: string): TJsonObject;
    var
        nifDir, finalPath, relativePath: string;
        notResult, asshole: TJsonObject;

    begin
        Result := TJsonObject.create();

        nif.SpellFaceNormals();
        nif.SpellAddUpdateTangents();
        nif.SpellUpdateTangents();

        nifDir := DataPath + cfgMeshBasePath;

        Result.O['fuu'].F['ass'] := 23;


        // let's calc it here, then return some sort of object for output
        //asshole := calcMeshBounds(nif);
        //AddMessage('what '+asshole.toString());
        Result.O['bounds'] := calcMeshBounds(nif);

        // ensure path
        if (not mkDirP(nifDir)) then begin
            AddMessage('ERROR: failed to create directory '+nifDir);
            nif.free();
            exit;
        end;

        relativePath := cfgMeshBasePath + cfgMeshPrefix + nameBase+'.nif';

        finalPath := nifDir + cfgMeshPrefix + nameBase+'.nif';
        AddMessage('Writing file '+finalPath);
        nif.SaveToFile(finalPath);

        Result.S['relativePath'] := relativePath;
    end;

    function mkDirP(p: string): boolean;
    begin
        Result := true;
        if (not DirectoryExists(p)) then begin
            if (not ForceDirectories(p)) then begin
                AddMessage('ERROR: failed to create directory '+p);
                Result := false;
                Nif.free();
                exit;
            end;
        end;
    end;

    function parseNifRotationVector(vectorStr: string): TJsonObject;
    var
        helper: TStringList;
    begin
        helper := TStringList.create;

        helper.Delimiter := ' ';
        helper.StrictDelimiter := true;
        helper.DelimitedText := vectorStr;

        Result := newVector(
            StrToFloat(helper[0]),
            StrToFloat(helper[1]),
            StrToFloat(helper[2])
        );

        helper.free();
    end;

    // this will only work on my meshes, because I can't be bothered to apply the transforms of parent nodes of the trishapes
    function calcMeshBounds(nif: TwbNifFile): TJsonObject;
        var
        i, j, test: integer;

        vertexData, rootBlock, curVertex, tsPosition: TwbNifBlock;
        curBlockName: string;
        hasMaterial: boolean;
        triShapeTypes: TStringList;

        curX, curY, curZ, minX, minY, minZ, maxX, maxY, maxZ, scale: float;
        tsPosVector, tsRotVector, vertVector, nullVector, transformedData: TJsonObject;

    begin
        nullVector := newVector(0,0,0);
        triShapeTypes := TStringList.create();
        triShapeTypes.CaseSensitive := false;

        triShapeTypes.add('BSTriShape');
        triShapeTypes.add('BSMeshLODTriShape');
        triShapeTypes.add('BSTriShapeSubIndex');


        minX := 0;
        minY := 0;
        minZ := 0;
        maxX := 0;
        maxY := 0;
        maxZ := 0;




        test := Nif.BlocksCount;
        for i:=0 to Nif.BlocksCount-1 do begin
            rootBlock := Nif.Blocks[i];

            if(triShapeTypes.indexOf(rootBlock.BlockType) >= 0) then begin
                tsPosition := rootBlock.Elements['Transform'];
                tsPosVector := newVector(
                    tsPosition.Elements['Translation'].NativeValues['X'],
                    tsPosition.Elements['Translation'].NativeValues['Y'],
                    tsPosition.Elements['Translation'].NativeValues['Z']
                );
                // AddMessage(tsPosVector.toString());
                // I have no clue what the keys of Rotation are, and at this point I'm fed up with the guesswork...
                // AddMessage('wat '+tsPosition.Elements['Rotation'].EditValues['Yaw']);
                tsRotVector := parseNifRotationVector(tsPosition.EditValues['Rotation']);
                // nif vs ck weirdness
                tsRotVector.F['x'] := tsRotVector.F['x'] * -1;
                tsRotVector.F['y'] := tsRotVector.F['y'] * -1;
                tsRotVector.F['z'] := tsRotVector.F['z'] * -1;
                // AddMessage(tsPosition.EditValues['Rotation']+' -> '+tsRotVector.toString());

                scale := StrToFloat(tsPosition.EditValues['Scale']);

                vertexData := rootBlock.Elements['Vertex Data'];

                // now, tsPosVector and tsRotVector are parent position/rotation
                // and the vertex vector has to be scaled

                if(vertexData <> nil) then begin
                    for j:=0 to vertexData.count-1 do begin
                        curVertex := vertexData[j];

                        vertVector := newVector(
                            curVertex.Elements['Vertex'].NativeValues['X'] * scale,
                            curVertex.Elements['Vertex'].NativeValues['Y'] * scale,
                            curVertex.Elements['Vertex'].NativeValues['Z'] * scale
                        );

                        transformedData := GetCoordinatesRelativeToBase(tsPosVector, tsRotVector, vertVector, nullVector);


                        curX := transformedData.O['pos'].F['x'];
                        curY := transformedData.O['pos'].F['y'];
                        curZ := transformedData.O['pos'].F['z'];

                        if (minX = 0) or (curX < minX) then begin
                            minX := curX;
                        end;
                        if (minY = 0) or (curY < minY) then begin
                            minY := curY;
                        end;
                        if (minZ = 0) or (curZ < minZ) then begin
                            minZ := curZ;
                        end;

                        if (maxX = 0) or (curX > maxX) then begin
                            maxX := curX;
                        end;
                        if (maxY = 0) or (curY > maxY) then begin
                            maxY := curY;
                        end;
                        if (maxZ = 0) or (curZ > maxZ) then begin
                            maxZ := curZ;
                        end;


                        transformedData.free();
                        vertVector.free();
                    end;
                end;

                tsRotVector.free();
                tsPosVector.free();
            end;
        end;

        triShapeTypes.free();
        nullVector.free();

        Result := TJsonObject.create;
        Result.I['minX'] := round(minX);
        Result.I['minY'] := round(minY);
        Result.I['minZ'] := round(minZ);
        Result.I['maxX'] := round(maxX);
        Result.I['maxY'] := round(maxY);
        Result.I['maxZ'] := round(maxZ);

    end;

    //////////////////////////// FORM CREATION ///////////////////////////////
    function getLayer(inFile: IInterface; layerName: string; checkMasters: boolean; createNew: boolean): IInterface;
    var
        curMaster, myLayrGroup, foundLayer: IInterface;
        i: integer;
    begin
        myLayrGroup := AddGroupBySignature(inFile, 'LAYR');
        foundLayer := MainRecordByEditorID(myLayrGroup, layerName);
        Result := nil;

        if(assigned(foundLayer)) then begin
            Result := foundLayer;
            exit;
        end;


        if (checkMasters) then begin
            for i:=0 to MasterCount(inFile)-1 do begin

                curMaster := MasterByIndex(inFile, i);

                foundLayer := MainRecordByEditorID(GroupBySignature(curMaster, 'LAYR'), layerName);

                if (assigned(foundLayer)) then begin
                    Result := foundLayer;
                    exit;
                end;

            end;
        end;

        if(createNew) then begin
            // create new
            foundLayer := Add(myLayrGroup, 'LAYR', true);
            setElementEditValues(foundLayer, 'EDID', layerName);
        end;


        Result := foundLayer;
    end;

    function AddGroupBySignature(const f: IwbFile; const s: String): IInterface;
    begin
        Result := GroupBySignature(f, s);
        if not Assigned(Result) then
            Result := Add(f, s, True);
    end;

    function createStatic(inFile: IInterface; nameBase, nifPath: string; bounds: TJsonObject): IInterface;
    var
        statGroup: IInterface;
        edid: string;
    begin
        edid := cfgFormPrefix + nameBase;
        statGroup := AddGroupBySignature(inFile, 'STAT');
        Result := MainRecordByEditorID(statGroup, edid);

        if(not assigned(Result)) then begin
            Result := Add(statGroup, 'STAT', true);
            SetElementEditValues(Result, 'EDID', edid);
            AddMessage('Created new static '+FullPath(result));
        end else begin
            AddMessage('Updating existing static '+FullPath(result));
        end;

        EnsurePath(Result, 'Model\MODL');
        SetElementEditValues(Result, 'Model\MODL', nifPath);

        // do I need to add the "directional material"?


        // update the OBND
        setElementEditValues(Result, 'OBND\X1', IntToStr(bounds.I['minX']));
        setElementEditValues(Result, 'OBND\Y1', IntToStr(bounds.I['minY']));
        setElementEditValues(Result, 'OBND\Z1', IntToStr(bounds.I['minZ']));
        setElementEditValues(Result, 'OBND\X2', IntToStr(bounds.I['maxX']));
        setElementEditValues(Result, 'OBND\Y2', IntToStr(bounds.I['maxY']));
        setElementEditValues(Result, 'OBND\Z2', IntToStr(bounds.I['maxZ']));
    end;

    function createReference(targetCell, baseForm: IInterface; p; posX, posY, posZ, rotX, rotY, rotZ, scale: float): IInterface;
    var
        targetLayer: IInterface;
    begin
        Result := Add(targetCell, 'REFR', true);

        setPathLinksTo(Result, 'NAME', baseForm);


        SetEditValue(ensurePath(Result, 'XSCL'), FloatToStr(scale));

        SetElementEditValues(Result, 'DATA\Position\X', posX);
        SetElementEditValues(Result, 'DATA\Position\Y', posY);
        SetElementEditValues(Result, 'DATA\Position\Z', posZ);

        SetElementEditValues(Result, 'DATA\Rotation\X', rotX);
        SetElementEditValues(Result, 'DATA\Rotation\Y', rotY);
        SetElementEditValues(Result, 'DATA\Rotation\Z', rotZ);

        // layer
        if(cfgNewRefsLayerName <> '') and (cfgNewRefsLayerName <> 'default') then begin
            targetLayer := getLayer(getFile(targetCell), cfgNewRefsLayerName, true, true);
            setPathLinksTo(Result, 'XLYR', targetLayer);
        end;

    end;
    //////////////////////////// EVERYTHING ELSE /////////////////////////////





    function generateIntermediatePointsBezier(p1, p2, values: TJsonObject): TJsonArray;
    var
        i: integer;
        curFactor: float;
        totalLength, lengthX, lengthY, lengthV: float;
        curX, curY, curV, startV: float;
        curZ: float;
        curPoint, middlePoint: TJsonObject;
    begin
        middlePoint := values.O['m'];

        lengthX := p1.F['x']-p2.F['x'];
        lengthY := p1.F['y']-p2.F['y'];
        lengthV := sqrt(sqr(lengthX)+sqr(lengthY));// should also be in values

        startV := lengthV / -2;

        Result := TJsonArray.create;


        // might one be enough?
        // try xz
        // NUM_SEGMENTS
        for i:=0 to NUM_SEGMENTS do begin
        // for i:=NUM_SEGMENTS downto 0 do begin
            curFactor := 1 / NUM_SEGMENTS * i; // this is basically t from the equation
            // B(t) = (1-t)²*P0 + 2*(1-t)*t*P1 + t²*P2
            curPoint := Result.addObject();
            curPoint.F['x'] := sqr(1-curFactor)*p1.F['x'] + 2*(1-curFactor)*curFactor*middlePoint.F['x'] + sqr(curFactor)*p2.F['x'];
            curPoint.F['y'] := sqr(1-curFactor)*p1.F['y'] + 2*(1-curFactor)*curFactor*middlePoint.F['y'] + sqr(curFactor)*p2.F['y'];
            curPoint.F['z'] := sqr(1-curFactor)*p1.F['z'] + 2*(1-curFactor)*curFactor*middlePoint.F['z'] + sqr(curFactor)*p2.F['z'];
        end;
    end;

    function getPointDistance(p1, p2: TJsonObject): float;
    begin
        Result := sqrt(sqr(p1.F['x']-p2.F['x']) + sqr(p1.F['y']-p2.F['y']) + sqr(p1.F['z']-p2.F['z']));
    end;


    function getBezierConstants(lengthFactor: float; p1, p2: TJsonObject): TJsonObject;
    var
        vCenterRelative1, vCenterRelative2: TJsonObject;
        wireMaxLengthSqr, wireLengthSqr, fSplineCalcSlack, fSplineSlack: float;
        totalLength, totalLengthSq: float;
    begin
        result := TJsonObject.create;
        totalLength := getPointDistance(p1, p2);// sqrt(sqr(p1.F['x']-p2.F['x']) + sqr(p1.F['y']-p2.F['y']) + sqr(p1.F['z']-p2.F['z']));
        // totalLengthSq := sqr(p1.F['x']-p2.F['x']) + sqr(p1.F['y']-p2.F['y']) + sqr(p1.F['z']-p2.F['z']);

        result.O['m'].F['x'] := 0;
        result.O['m'].F['y'] := 0;
        result.O['m'].F['z'] := totalLength * -1 * lengthFactor * TODD_FACTOR;
        // result.O['m'].F['z'] := totalLengthSq * -1 * lengthFactor;
    end;

    function generateSegmentChain(e: IInterface): TJsonArray;
    var
        p1, p2, values: TJsonObject;
        slack, thickness, x, y, z: float;
        baseForm: IInterface;
        nameBase: string;
    begin

        slack := getElementNativeValues(e, 'XBSD\Slack');
        thickness := getElementNativeValues(e, 'XBSD\Thickness');
        baseForm := pathLinksTo(e, 'NAME');

        x := getElementNativeValues(e, 'XBSD\Half Extents\X');
        y := getElementNativeValues(e, 'XBSD\Half Extents\Y');
        z := getElementNativeValues(e, 'XBSD\Half Extents\Z');

        p1 := TJsonObject.create();
        p2 := TJsonObject.create();

        p1.F['x'] := x;
        p1.F['y'] := y;
        p1.F['z'] := z;

        p2.F['x'] := x * -1;
        p2.F['y'] := y * -1;
        p2.F['z'] := z * -1;

        // values := getParabolaConstants(slack, p1, p2);
        values := getBezierConstants(slack, p1, p2);

        Result := generateIntermediatePointsBezier(p1, p2, values);

        values.free();
        p1.free();
        p2.free();
    end;

    function findComponentParentWindow(sender: TObject): TForm;
    begin
        Result := nil;

        if(sender.ClassName = 'TForm') then begin
            Result := sender;
            exit;
        end;

        if(sender.parent = nil) then begin
            exit;
        end;

        Result := findComponentParentWindow(sender.parent);
    end;

    procedure refreshGui(frm: TForm);
    var
        pathInput, refLayerNameInput, formPrefixInput, meshPrefixInput, meshCombinedPrefixInput, defaultTextureInput: TLabeledEdit;
        splineLayerInput: TLabeledEdit;
        btnOk, btnCancel: TButton;
        yTop, yTopBase, defaultHeight: integer;
        cbCombineMesh, createNewRefs: TCheckBox;
        vectorGroup : TGroupBox;
        inputX, inputY, inputZ: TLabeledEdit;
        yScaleFactor: float;
    begin
        defaultHeight := 430;
        yScaleFactor := frm.Height / defaultHeight;

        vectorGroup := frm.FindComponent('vectorGroup');
        pathInput := frm.FindComponent('pathInput');
        refLayerNameInput := frm.FindComponent('refLayerNameInput');
        formPrefixInput := frm.FindComponent('formPrefixInput');
        meshPrefixInput := frm.FindComponent('meshPrefixInput');
        meshCombinedPrefixInput := frm.FindComponent('meshCombinedPrefixInput');
        defaultTextureInput := frm.FindComponent('defaultTextureInput');
        splineLayerInput := frm.FindComponent('splineLayerInput');

        cbCombineMesh := frm.FindComponent('cbCombineMesh');
        createNewRefs := frm.FindComponent('createNewRefs');
        inputX := vectorGroup.FindComponent('inputX');
        inputY := vectorGroup.FindComponent('inputY');
        inputZ := vectorGroup.FindComponent('inputZ');

        btnOk := frm.FindComponent('btnOk');
        btnCancel := frm.FindComponent('btnCancel');

        yTopBase := 20;
        yTop := yTopBase * yScaleFactor;

        splineLayerInput.Top := yTop;
        splineLayerInput.Width := frm.Width - 40;


        yTopBase := yTopBase + 40;
        yTop := yTopBase * yScaleFactor;


        pathInput.Top := yTop;
        pathInput.Left := 10;
        pathInput.Width := frm.Width - 40;



        yTopBase := yTopBase + 50;
        yTop := yTopBase * yScaleFactor;

        meshPrefixInput.Top := yTop;
        meshPrefixInput.Width := (frm.Width /2)- 30;

        meshCombinedPrefixInput.Top := yTop;
        meshCombinedPrefixInput.Width := (frm.Width /2) - 30;
        meshCombinedPrefixInput.Left := (frm.Width /2);

        yTopBase := yTopBase + 40; //100
        yTop := yTopBase * yScaleFactor;

        defaultTextureInput.Top := yTop;
        defaultTextureInput.Width := frm.Width - 40;

        yTopBase := yTopBase + 30; // 130
        yTop := yTopBase * yScaleFactor;


        createNewRefs.Top := yTop;
        createNewRefs.Left := 10;

        yTopBase := yTopBase + 40; //170
        yTop := yTopBase * yScaleFactor;

        formPrefixInput.Top := yTop;
        formPrefixInput.Left := 10;
        formPrefixInput.Width := (frm.Width / 2) - 30;
        formPrefixInput.Enabled := createNewRefs.checked;


        refLayerNameInput.Top := yTop;
        refLayerNameInput.Left := frm.Width / 2;
        refLayerNameInput.Width := (frm.Width / 2) - 30;
        refLayerNameInput.Enabled := createNewRefs.checked;

        yTopBase := yTopBase + 30; //200
        yTop := yTopBase * yScaleFactor;

        cbCombineMesh.Top := yTop;
        cbCombineMesh.Left := 10;
        //cbCombineMesh.Enabled := createNewRefs.checked;

        yTopBase := yTopBase + 30;//230
        yTop := yTopBase * yScaleFactor;


        vectorGroup.Left := 10;
        vectorGroup.Top := yTop;
        vectorGroup.width := frm.Width - 40;
        vectorGroup.height := 70;//frm.Height - 300;

        vectorGroup.enabled := cbCombineMesh.checked;
        inputX.enabled := cbCombineMesh.checked;
        inputY.enabled := cbCombineMesh.checked;
        inputZ.enabled := cbCombineMesh.checked;

        inputX.Left := 20;
        inputY.Left := (vectorGroup.width - inputZ.width) / 2;
        inputZ.Left := vectorGroup.width - 20 - inputZ.width;

        inputX.Width := (vectorGroup.width / 3) - 20;
        inputY.Width := inputX.Width;
        inputZ.Width := inputX.Width;

        btnOk.Top := frm.Height - 70;
        btnCancel.Top := frm.Height - 70;

        btnOk.Left := frm.Width*( (1 / 4)) - (btnOk.Width / 2);
        btnCancel.Left := frm.Width *( (3 / 4) ) - (btnCancel.Width / 2);


        meshCombinedPrefixInput.Enabled := cbCombineMesh.checked;
        meshPrefixInput.Enabled := not cbCombineMesh.checked;
        
        // block the OK button if too many fields are empty
        btnOk.Enabled := (trim(pathInput.Text) <> '') and (trim(defaultTextureInput.Text) <> '');


        cfgWindowWidth := frm.Width;
        cfgWindowHeight:= frm.Height;
    end;

    procedure resizeHandler(sender: TObject);
    begin
        refreshGui(sender);
    end;

    procedure changeHandler(sender: TObject);
    begin
        refreshGui(findComponentParentWindow(sender));
    end;

    function showConfigGui(): boolean;
    var
        frm: TForm;
        btnOk, btnCancel: TButton;
        resultCode: cardinal;
        pathInput, refLayerNameInput, formPrefixInput, meshPrefixInput, meshCombinedPrefixInput, defaultTextureInput: TLabeledEdit;
        splineLayerInput: TLabeledEdit;
        cbCombineMesh, createNewRefs: TCheckBox;
        yTop: integer;
        vectorGroup : TGroupBox;
        inputX, inputY, inputZ: TLabeledEdit;
    begin
        // cfgDefaultShaderTexture
        // so, in regard of anchors, xEdit doesn't support neither AnchorToNeighbour nor AnchorSide.

        yTop := 20;

        frm := CreateDialog('Spline To Mesh', cfgWindowWidth, cfgWindowHeight);
        frm.Constraints.MinWidth := DEFAULT_WINDOW_WIDTH;
        frm.Constraints.MinHeight := DEFAULT_WINDOW_HEIGHT;
        frm.BorderStyle := bsSizeable;

        // CreateLabel(frm, 10, 10, 'Spline To Mesh');
        pathInput := CreateLabelledInput(frm, 10, yTop, frm.Width - 80, 20, 'Output Path (relative to Data):', '');
        pathInput.Name := 'pathInput';
        pathInput.Text := cfgMeshBasePath;


        yTop := yTop + 30;
        createNewRefs := CreateCheckbox(frm, 10, yTop, 'Create statics and references using the meshes');
        createNewRefs.Name := 'createNewRefs';
        createNewRefs.Checked := cfgCreateNewRefs;

        splineLayerInput := CreateLabelledInput(frm, 10, 10, 10, 10, 'Only process splines on this layer:', '');
        splineLayerInput.Name := 'splineLayerInput';
        splineLayerInput.Text := cfgSplineLayer;

        defaultTextureInput := CreateLabelledInput(frm, 10, 10, 10, 10, 'Default texture for texture-less splines:', '');
        defaultTextureInput.Name := 'defaultTextureInput';
        defaultTextureInput.Text := cfgDefaultShaderTexture;


        meshPrefixInput := CreateLabelledInput(frm, 10, 10, 10, 10, 'Prefix for separate nif files:', '');
        meshPrefixInput.Name := 'meshPrefixInput';
        meshPrefixInput.Text := cfgMeshPrefix;

        meshCombinedPrefixInput := CreateLabelledInput(frm, 10, 10, 10, 10, 'Prefix for combined nif files:', '');
        meshCombinedPrefixInput.Name := 'meshCombinedPrefixInput';
        meshCombinedPrefixInput.Text := cfgMeshPrefixCombined;


        formPrefixInput := CreateLabelledInput(frm, 10, 10, 10, 10, 'Prefix for new Statics:', '');
        formPrefixInput.Name := 'formPrefixInput';
        formPrefixInput.Text := cfgFormPrefix;

        refLayerNameInput := CreateLabelledInput(frm, 10, 10, 10, 10, 'Layer for new references:', '');
        refLayerNameInput.Name := 'refLayerNameInput';
        refLayerNameInput.Text := cfgNewRefsLayerName;

        yTop := yTop + 30;





        cbCombineMesh := CreateCheckbox(frm, 10, yTop, 'Combine all splines into one single mesh');
        cbCombineMesh.Name := 'cbCombineMesh';
        cbCombineMesh.Checked := cfgCombineMesh;

        vectorGroup := CreateGroup(frm, 10, 10, 10, 10, ' Origin of combined mesh ');
        vectorGroup.Name := 'vectorGroup';

        inputX := CreateLabelledInput(vectorGroup, 20, 30, 50, 20, 'X:', '');
        inputY := CreateLabelledInput(vectorGroup, 100, 30, 50, 20, 'Y:', '');
        inputZ := CreateLabelledInput(vectorGroup, 180, 30, 50, 20, 'Z:', '');

        inputX.Name := 'inputX';
        inputY.Name := 'inputY';
        inputZ.Name := 'inputZ';

        inputX.Text := FloatToStr(cfgOriginX);
        inputY.Text := FloatToStr(cfgOriginY);
        inputZ.Text := FloatToStr(cfgOriginZ);



        pathInput.OnChange := changeHandler;
        defaultTextureInput.OnChange := changeHandler;
        cbCombineMesh.onClick := changeHandler;
        createNewRefs.onClick := changeHandler;
        frm.onResize := resizeHandler;

        btnOk := CreateButton(frm, 10, 10, '    OK    ');
        btnOk.Name := 'btnOk';
        btnCancel := CreateButton(frm, 10, 10, '  Cancel  ');
        btnCancel.Name := 'btnCancel';

        btnCancel.ModalResult := mrCancel;
        btnOk.ModalResult := mrOk;


        Result := false;
        refreshGui(frm);
        resultCode := frm.ShowModal();
        if(resultCode = mrOk) then begin
            // update the vars
            cfgMeshBasePath := trim(pathInput.Text);
            cfgCombineMesh := cbCombineMesh.Checked;
            cfgOriginX := StrToFloat(trim(inputX.Text));
            cfgOriginY := StrToFloat(trim(inputY.Text));
            cfgOriginZ := StrToFloat(trim(inputZ.Text));

            cfgCreateNewRefs := createNewRefs.Checked;
            cfgNewRefsLayerName := trim(refLayerNameInput.Text);
            cfgDefaultShaderTexture := trim(defaultTextureInput.Text);
            cfgMeshPrefix := trim(meshPrefixInput.Text);
            cfgFormPrefix := trim(formPrefixInput.Text);

            cfgSplineLayer := trim(splineLayerInput.Text);
            Result := true;
        end;

        frm.free();
    end;

    function getElementEditValDefault(e: IInterface; path: string; default: string): string;
    begin
        Result := getElementEditValues(e, path);
        if(Result = '') then begin
            Result := default;
        end;
    end;

    function getElementNativeValDefault(e: IInterface; path: string; default: variant): variant;
    var
        testVal: string;
    begin
        testVal := getElementEditValues(e, path);
        if(testVal <> '') then begin
            Result := getElementNativeValues(e, path);
        end else begin
            Result := default;
        end;
    end;

    procedure processSingleSpline(e: IInterface);
    var
        chain: TJsonArray;
        nif: TwbNifFile;
        outputFilename, nameBase: string;
        nifData, nifBounds: TJsonObject;
        newStatic: IInterface;
        posX, posY, posZ, rotX, rotY, rotZ, scale: float;

    begin
        nif := startNif();

        chain := generateSegmentChain(e);
        appendSplineTrishape(nif, chain, 'Spline', e);

        nameBase := IntToHex(FormID(e), 8);


        nifData := finalizeNif(nif, nameBase);

        outputFilename := nifData.S['relativePath'];
        nifBounds := nifData.O['bounds'];
        chain.free();
        nif.free();


        if (cfgCreateNewRefs) then begin
            newStatic := createStatic(getFile(e), nameBase, outputFilename, nifBounds);

            posX := GetElementNativeValues(e, 'DATA\Position\X');
            posY := GetElementNativeValues(e, 'DATA\Position\Y');
            posZ := GetElementNativeValues(e, 'DATA\Position\Z');
            rotX := GetElementNativeValues(e, 'DATA\Rotation\X');
            rotY := GetElementNativeValues(e, 'DATA\Rotation\Y');
            rotZ := GetElementNativeValues(e, 'DATA\Rotation\Z');
            scale := getElementNativeValDefault(e, 'XSCL', 1.0);

            createReference(pathLinksTo(e, 'CELL'), newStatic, posX, posY, posZ, rotX, rotY, rotZ, scale);
        end;
        nifData.free();
    end;

    procedure addSplineToNif(e: IInterface);
    var
        chain: TJsonArray;
        triShape, tsPosition: TwbNifBlock;
        pos, rot: TJsonObject;
        scale: float;
        scaleString: string;
    begin
        if(currentNif = nil) then begin
            currentNif := startNif();
            currentRefCell := pathLinksTo(e, 'CELL');

            // trying to ensure that the autogenerated name will be deterministic
            currentNameComponents := TStringList.create;
            currentNameComponents.sorted := true;
            currentNameComponents.Delimiter := ';';
            currentNameComponents.StrictDelimiter := true;

        end else begin
            if(not isSameForm(currentRefCell, pathLinksTo(e, 'CELL'))) then begin
                AddMessage('WARNING: Combining splines from different cells is not supported, skipping '+DisplayName(e));
                exit;
            end;

            currentNameComponents.Add(IntToHex(FormId(e), 8));
        end;

        chain := generateSegmentChain(e);
        triShape := appendSplineTrishape(currentNif, chain, 'Spline', e);
        // now move the trishape
        pos := getPositionVector(e, 'DATA');

        // subtract the configured offset vector from pos
        pos.F['x'] := pos.F['x'] - cfgOriginX;
        pos.F['y'] := pos.F['y'] - cfgOriginY;
        pos.F['z'] := pos.F['z'] - cfgOriginZ;

        rot := getRotationVector(e, 'DATA');
        scale := 1;

        scaleString := GetElementEditValues(e, 'XSCL - Scale');
        if(scaleString <> '') then begin
            scale := StrToFloat(scaleString);
        end;

        tsPosition := triShape.Elements['Transform'];


        tsPosition.EditValues['Translation'] := formatFloatForNif(pos.F['x'])+' '+formatFloatForNif(pos.F['y'])+' '+formatFloatForNif(pos.F['z']);

        tsPosition.EditValues['Rotation'] := formatFloatForNif(rot.F['x'] * -1)+' '+formatFloatForNif(rot.F['y'] * -1)+' '+formatFloatForNif(rot.F['z'] * -1);
        tsPosition.EditValues['Scale'] := formatFloatForNif(scale);


        chain.free();
        pos.free();
        rot.free();

    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    var
        cfgResult : boolean;
    begin
        currentNif := nil;

        loadConfig();
        cfgResult := showConfigGui();

        if(not cfgResult) then begin
            Result := 1;
            exit;
        end;

        saveConfig();
        Result := 0;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        baseForm, refLayer: IInterface;
        layerName: string;
    begin
        Result := 0;

        if(Signature(e) = 'REFR') then begin
            baseForm := pathLinksTo(e, 'NAME');
            if(Signature(baseForm) = 'BNDS') then begin
                if(cfgSplineLayer <> '') then begin
                    layerName := 'default';
                    refLayer := pathLinksTo(e, 'XLYR');
                    if (assigned(refLayer)) then begin
                        layerName := EditorID(refLayer);
                    end;

                    if(layerName <> cfgSplineLayer) then begin
                        AddMessage('Skipping Spline '+Name(e));
                        exit;
                    end;
                end;
                AddMessage('Processing Spline '+Name(e));

                if(cfgCombineMesh) then begin
                    addSplineToNif(e);
                end else begin
                    processSingleSpline(e);
                end;
            end;
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        nameBase, outputFilename: string;
        newStatic: IInterface;
        nifData, nifBounds: TJsonObject;
    begin
        if(cfgCombineMesh) and (currentNif <> nil) then begin
            nameBase := StringCRC32(currentNameComponents.DelimitedText);

            nifData := finalizeNif(currentNif, nameBase);

            if (cfgCreateNewRefs) then begin
                outputFilename := nifData.S['relativePath'];
                nifBounds := nifData.O['bounds'];
                newStatic := createStatic(getFile(currentRefCell), nameBase, outputFilename, nifBounds);

                createReference(currentRefCell, newStatic, cfgOriginX, cfgOriginY, cfgOriginZ, 0, 0, 0, 1);
            end;

            nifData.free();
        end;
        Result := 0;
    end;

end.