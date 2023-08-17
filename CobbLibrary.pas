{
    Library for handling vectors and rotations.

    Originally implemented in papyrus as CobbLibraryRotations.psc and CobbLibraryVectors.psc by David J Cobb.

    Ported to xEdit pascal by Pra.

    "Here be bugs"
    ==============
    I have ported this code from Papyrus to xEdit. I am not good enough at math to verify that I did it correctly.
}
unit CobbLibrary;

    uses XeditSimpleMath;

    ///// CONTRUCTORS //////

    function newVector(x, y, z: float): TJsonObject;
    begin
        Result := TJsonObject.create();

        Result.F['x'] := x;
        Result.F['y'] := y;
        Result.F['z'] := z;
    end;

    function newQuaternion(w, x, y, z: float): TJsonObject;
    begin
        Result := TJsonObject.create();

        Result.F['w'] := w;
        Result.F['x'] := x;
        Result.F['y'] := y;
        Result.F['z'] := z;
    end;

    function newAxisAngle(x, y, z, angle: float): TJsonObject;
    begin
        Result := TJsonObject.create();

        Result.F['x'] := x;
        Result.F['y'] := y;
        Result.F['z'] := z;
        Result.F['angle'] := angle;
    end;


    function newMatrix(a, b, c, d, e, f, g, h, i: float): TJsonArray;
    begin
        {Matrix indices are:
        0 1 2
        3 4 5
        6 7 8 }
        Result := TJsonArray.create;

        Result.add(a);
        Result.add(b);
        Result.add(c);

        Result.add(d);
        Result.add(e);
        Result.add(f);

        Result.add(g);
        Result.add(h);
        Result.add(i);
    end;
    ////// UTILS ////////

    function indexToVectorComponent(i: integer): string;
    begin
        Result := '';
        case i of
            0: Result := 'x';
            1: Result := 'y';
            2: Result := 'z';
        end;
    end;

    ////// VECTORS ///////
    { Multiplies a vector by -1 and returns the result as a new vector. }
    function VectorNegate(av: TJsonObject): TJsonObject;
    begin
        result := VectorMultiply(av, -1.0);
    end;

    { Projects one vector onto another, returning the result as a new vector. }
    Function VectorProject(avA, avB: TJsonObject): TJsonObject;
    var
        scalar: float;
    begin
        {
        float[] vOut = new float[3]
        vOut[0] = avB[0]
        vOut[1] = avB[1]
        vOut[2] = avB[2]
        float scalar = AutoBuilder:CobbLibraryVectors.VectorDotProduct(avA, avB) / AutoBuilder:CobbLibraryVectors.VectorDotProduct(avB, avB)
        return AutoBuilder:CobbLibraryVectors.VectorMultiply(vOut, scalar)
        }
        scalar := VectorDotProduct(avA, avB) / VectorDotProduct(avB, avB);
        Result := VectorMultiply(avB, scalar);
    end;

    { Divides a vector by a scalar and returns the result as a new vector. }
    Function VectorDivide(avA :TJsonObject; afB: float) : TJsonObject;
    begin
        {
        float[] vOut = new float[3]
        If (afB == 0 as float)
            Debug.TraceStack("VectorDivide: A script asked me to divide a vector by zero. I just returned a null vector instead.", 1)
            return vOut
        EndIf
        vOut[0] = avA[0] / afB
        vOut[1] = avA[1] / afB
        vOut[2] = avA[2] / afB
        return vOut
        }
        Result := newVector(0,0,0);

        If (afB = 0.0) then begin
            AddMessage('VectorDivide: A script asked me to divide a vector by zero. I just returned a null vector instead.');
            exit;
        end;
        Result.F['x'] := avA['x'] / afB;
        Result.F['y'] := avA['y'] / afB;
        Result.F['z'] := avA['z'] / afB;
    end;

    { Takes the cross product of two vectors and returns the result as a new vector. }
    Function VectorCrossProduct(avA, avB: TJsonObject): TJsonObject;
    begin
        {
            float[] vOut = new float[3]
            vOut[0] = avA[1] * avB[2] - avA[2] * avB[1]
            vOut[1] = avA[2] * avB[0] - avA[0] * avB[2]
            vOut[2] = avA[0] * avB[1] - avA[1] * avB[0]
            return vOut
        }
        Result := newVector(
            avA.F['y'] * avB.F['z'] - avA.F['z'] * avB.F['y'],
            avA.F['z'] * avB.F['x'] - avA.F['x'] * avB.F['z'],
            avA.F['x'] * avB.F['y'] - avA.F['y'] * avB.F['x']
        );
    end;

    { Subtracts one vector from another and returns the difference as a new vector. }
    Function VectorSubtract(avA, avB: TJsonObject): TJsonObject;
    begin
        Result := newVector(
            avA.F['x'] - avB.F['x'],
            avA.F['y'] - avB.F['y'],
            avA.F['z'] - avB.F['z']
        );
    end;

    { Returns the dot product of two vectors. }
    Function VectorDotProduct(avA, avB: TJsonObject): float;
    begin
        {
        fOut += avA[0] * avB[0]
        fOut += avA[1] * avB[1]
        fOut += avA[2] * avB[2]
        }
        Result := avA.F['x'] * avB.F['x'] + avA.F['y'] * avB.F['y'] + avA.F['z'] * avB.F['z'];
    end;

    { Adds two vectors together and returns the sum as a new vector. }
    Function VectorAdd(avA, avB: TJsonObject): TJsonObject;
    begin
        Result := newVector(
            avA.F['x'] + avB.F['x'],
            avA.F['y'] + avB.F['y'],
            avA.F['z'] + avB.F['z']
        );
    end;

    { Returns the length of a vector. }
    function VectorLength(av: TJsonObject): float;
    begin
        Result := sqrt(av.F['x'] * av.F['x'] + av.F['y'] * av.F['y'] + av.F['z'] * av.F['z'])
    end;

    { Multiplies a vector by a scalar and returns the result as a new vector. }
    Function VectorMultiply(avA: TJsonObject; afB: float): TJsonObject;
    begin
        Result := newVector(
            avA.F['x'] * afB,
            avA.F['y'] * afB,
            avA.F['z'] * afB
        );

    end;

    { Normalizes a vector and returns the result as a new vector. }
    Function VectorNormalize(av: TJsonObject): TJsonObject;
    begin;
        Result := VectorDivide(av, VectorLength(av));
    end;

    /////// ROTATIONS //////////
    { Converts an axis-angle orientation [x, y, z, angle] to a unit quaternion (versor), returning [w, x, y, z]. }
    Function AxisAngleToQuaternion(afAxisAngle: TJsonObject) : TJsonObject;
    var
        fHalfAngle: float;
    begin;
        {
            float[] qOutput = new float[4]
            float fHalfAngle = afAxisAngle[3] / 2 as float
            qOutput[0] = Math.cos(fHalfAngle)
            qOutput[1] = Math.sin(fHalfAngle) * afAxisAngle[0]
            qOutput[2] = Math.sin(fHalfAngle) * afAxisAngle[1]
            qOutput[3] = Math.sin(fHalfAngle) * afAxisAngle[2]
        }
        fHalfAngle := afAxisAngle.F['angle'] / 2.0;
        Result := newQuaternion(
            cosDeg(fHalfAngle),
            sinDeg(fHalfAngle) * afAxisAngle.F['x'],
            sinDeg(fHalfAngle) * afAxisAngle.F['y'],
            sinDeg(fHalfAngle) * afAxisAngle.F['z']
        );

    end;

    { Converts a set of Euler angles to axis angle, returning [x, y, z, angle]. The angle is in degrees. Tailored for Skyrim (extrinsic left-handed ZYX Euler). }
    Function EulerToAxisAngle(afX, afY, afZ: float): TJsonObject;
    var
        fMatrix: TJsonObject;
    begin
        fMatrix := EulerToMatrix(afX, afY, afZ);
        Result := MatrixToAxisAngle(fMatrix);

        fMatrix.free();
    end;

    { Adds two quaternions, returning the result as a new quaternion. }
    Function QuaternionAdd(aqA, aqB: TJsonObject) : TJsonObject;
	begin
        Result := newQuaternion(
            aqA.F['w'] + aqB.F['w'],
            aqA.F['x'] + aqB.F['x'],
            aqA.F['y'] + aqB.F['y'],
            aqA.F['z'] + aqB.F['z']
        );
	end;

    {
        Converts a set of Euler angles to a rotation matrix. Tailored for Skyrim (extrinsic left-handed ZYX Euler).
        Matrix indices are:
        0 1 2
        3 4 5
        6 7 8
    }
    Function EulerToMatrix(afX, afY, afZ: float): TJsonArray;
    var
        fSinX, fSinY, fSinZ, fCosX, fCosY, fCosZ: float;
    begin

        {
            float[] fOutput = new float[9]
            float fSinX = Math.sin(afX)
            float fSinY = Math.sin(afY)
            float fSinZ = Math.sin(afZ)
            float fCosX = Math.cos(afX)
            float fCosY = Math.cos(afY)
            float fCosZ = Math.cos(afZ)
            fOutput[0] = fCosY * fCosZ
            fOutput[1] = fCosY * fSinZ
            fOutput[2] = -fSinY
            fOutput[3] = fSinX * fSinY * fCosZ - fCosX * fSinZ
            fOutput[4] = fSinX * fSinY * fSinZ + fCosX * fCosZ
            fOutput[5] = fSinX * fCosY
            fOutput[6] = fCosX * fSinY * fCosZ + fSinX * fSinZ
            fOutput[7] = fCosX * fSinY * fSinZ - fSinX * fCosZ
            fOutput[8] = fCosX * fCosY
            return fOutput
        }
        fSinX := sinDeg(afX);
        fSinY := sinDeg(afY);
        fSinZ := sinDeg(afZ);
        fCosX := cosDeg(afX);
        fCosY := cosDeg(afY);
        fCosZ := cosDeg(afZ);

        Result := newMatrix(
            fCosY * fCosZ,
            fCosY * fSinZ,
            -fSinY,
            fSinX * fSinY * fCosZ - fCosX * fSinZ,
            fSinX * fSinY * fSinZ + fCosX * fCosZ,
            fSinX * fCosY,
            fCosX * fSinY * fCosZ + fSinX * fSinZ,
            fCosX * fSinY * fSinZ - fSinX * fCosZ,
            fCosX * fCosY
        );

    end;

    { Converts a unit quaternion (versor) to an axis-angle representation, returning [x, y, z, angle]. }
    Function QuaternionToAxisAngle(aqQuat: TJsonObject) : TJsonObject;
    var
        matrix: TJsonArray;
    begin
        matrix := QuaternionToMatrix(aqQuat);
        Result := MatrixToAxisAngle(matrix);

        matrix.free();
    end;

    { Returns the trace of a 3x3 rotation matrix. }
    Function MatrixTrace(afMatrix: TJsonArray): float;
    begin
        Result := afMatrix.F[0] + afMatrix.F[4] + afMatrix.F[8];
    end;

    {
        Converts a quaternion (as [w, x, y, z]) to a rotation matrix.

        NOTE: I have not tested to see whether using a unit quaternion or a non-normalized quaternion makes any difference.
    }
    Function QuaternionToMatrix(aqQuat: TJsonObject): TJsonArray;
    begin
{
	int W = 0
	int x = 1
	int y = 2
	int Z = 3
	float[] mOutput = new float[9]
	mOutput[0] = 1 as float - 2 as float * Math.pow(aqQuat[y], 2 as float) - 2 as float * Math.pow(aqQuat[Z], 2 as float)
	mOutput[1] = 2 as float * aqQuat[x] * aqQuat[y] - 2 as float * aqQuat[Z] * aqQuat[W]
	mOutput[2] = 2 as float * aqQuat[x] * aqQuat[Z] + 2 as float * aqQuat[y] * aqQuat[W]
	mOutput[3] = 2 as float * aqQuat[x] * aqQuat[y] + 2 as float * aqQuat[Z] * aqQuat[W]
	mOutput[4] = 1 as float - 2 as float * Math.pow(aqQuat[x], 2 as float) - 2 as float * Math.pow(aqQuat[Z], 2 as float)
	mOutput[5] = 2 as float * aqQuat[y] * aqQuat[Z] - 2 as float * aqQuat[x] * aqQuat[W]
	mOutput[6] = 2 as float * aqQuat[x] * aqQuat[Z] - 2 as float * aqQuat[y] * aqQuat[W]
	mOutput[7] = 2 as float * aqQuat[y] * aqQuat[Z] + 2 as float * aqQuat[x] * aqQuat[W]
	mOutput[8] = 1 as float - 2 as float * Math.pow(aqQuat[x], 2 as float) - 2 as float * Math.pow(aqQuat[y], 2 as float)
	return mOutput
}
        Result := newMatrix(
            1.0 - 2.0 * sqr(aqQuat.F['y']) - 2.0 * sqr(aqQuat.F['z']),
            2.0 * aqQuat.F['x'] * aqQuat.F['y'] - 2.0 * aqQuat.F['z'] * aqQuat.F['w'],
            2.0 * aqQuat.F['x'] * aqQuat.F['z'] + 2.0 * aqQuat.F['y'] * aqQuat.F['w'],
            2.0 * aqQuat.F['x'] * aqQuat.F['y'] + 2.0 * aqQuat.F['z'] * aqQuat.F['w'],
            1.0 - 2.0 * sqr(aqQuat.F['x']) - 2.0 * sqr(aqQuat.F['z']),
            2.0 * aqQuat.F['y'] * aqQuat.F['z'] - 2.0 * aqQuat.F['x'] * aqQuat.F['w'],
            2.0 * aqQuat.F['x'] * aqQuat.F['z'] - 2.0 * aqQuat.F['y'] * aqQuat.F['w'],
            2.0 * aqQuat.F['y'] * aqQuat.F['z'] + 2.0 * aqQuat.F['x'] * aqQuat.F['w'],
            1.0 - 2.0 * sqr(aqQuat.F['x']) - 2.0 * sqr(aqQuat.F['y'])
        );
    end;

    { Multiplies a matrix by a column vector, and returns the resulting column vector. }
    Function MatrixMultiplyByColumn(amMatrix: TJsonArray; avColumn: TJsonObject): TJsonObject;
    begin
        {
        vResult[0] = amMatrix[0] * avColumn[0] + amMatrix[1] * avColumn[1] + amMatrix[2] * avColumn[2]
        vResult[1] = amMatrix[3] * avColumn[0] + amMatrix[4] * avColumn[1] + amMatrix[5] * avColumn[2]
        vResult[2] = amMatrix[6] * avColumn[0] + amMatrix[7] * avColumn[1] + amMatrix[8] * avColumn[2]
        }
        Result := newVector(
            amMatrix.F[0] * avColumn.F['x'] + amMatrix.F[1] * avColumn.F['y'] + amMatrix.F[2] * avColumn.F['z'],
            amMatrix.F[3] * avColumn.F['x'] + amMatrix.F[4] * avColumn.F['y'] + amMatrix.F[5] * avColumn.F['z'],
            amMatrix.F[6] * avColumn.F['x'] + amMatrix.F[7] * avColumn.F['y'] + amMatrix.F[8] * avColumn.F['z']
        );
    end;

    function MatrixMultiply(m1, m2: TJsonArray): TJsonArray;
    var
        a, b, c, d, e, f, g, h, i, j, k, l, m, n, p, q, r, s: Float;
    begin
        {
            (a b c)   (j k l)   (a*j+b*m+c*q   a*k+b*n+c*r   a*l+b*p+c*s)
            (d e f) x (m n p) = (d*j+e*m+f*q   d*k+e*n+f*r   d*l+e*p+f*s)
            (g h i)   (q r s)   (g*j+h*m+i*q   g*k+h*n+i*r   g*l+h*p+i*s)

            0 1 2
            3 4 5
            6 7 8
        }

        a := m1.F[0];
        b := m1.F[1];
        c := m1.F[2];
        d := m1.F[3];
        e := m1.F[4];
        f := m1.F[5];
        g := m1.F[6];
        h := m1.F[7];
        i := m1.F[8];

        j := m2.F[0];
        k := m2.F[1];
        l := m2.F[2];
        m := m2.F[3];
        n := m2.F[4];
        p := m2.F[5];
        q := m2.F[6];
        r := m2.F[7];
        s := m2.F[8];

        Result := newMatrix(
            a*j+b*m+c*q, a*k+b*n+c*r, a*l+b*p+c*s,
            d*j+e*m+f*q, d*k+e*n+f*r, d*l+e*p+f*s,
            g*j+h*m+i*q, g*k+h*n+i*r, g*l+h*p+i*s
        );
    end;

	{
		Multiplies a matrix by a scalar
	}
	function MatrixMultiplyScalar(scalar: float; matrix: TJsonObject): TJsonObject;
	begin
		Result := newMatrix(
			scalar * matrix.F[0], scalar * matrix.F[1], scalar * matrix.F[2],
			scalar * matrix.F[3], scalar * matrix.F[4], scalar * matrix.F[5],
			scalar * matrix.F[6], scalar * matrix.F[7], scalar * matrix.F[8]
		);
	end;

	{
		Calculates the determinant of a matrix
	}
	function MatrixDeterminant(matrix: TJsonObject): float;
	begin
		{
		| 0 1 2 |
		| 3 4 5 |
		| 6 7 8 |
		}
		// https://en.wikipedia.org/wiki/Rule_of_Sarrus
		Result :=
			  matrix.F[0]*matrix.F[4]*matrix.F[8] + matrix.F[1]*matrix.F[5]*matrix.F[6] + matrix.F[2]*matrix.F[3]*matrix.F[7]
			- matrix.F[6]*matrix.F[4]*matrix.F[2] - matrix.F[7]*matrix.F[5]*matrix.F[0] - matrix.F[8]*matrix.F[3]*matrix.F[1];
	end;

	{
		Calculates the inverse of a matrix. Warning: this will return nil, if the matrix is non-invertible (determinant = 0)
	}
	function InvertMatrix(matrix: TJsonObject): TJsonObject;
	var
		det, a, b, c, d, e, f, g, h, i: float;
		tempMatrix: TJsonObject;
	begin
		det := MatrixDeterminant(matrix);
		if(det = 0) then begin
			Result := nil;
			exit;
		end;

		a := matrix.F[0];
        b := matrix.F[1];
        c := matrix.F[2];
        d := matrix.F[3];
        e := matrix.F[4];
        f := matrix.F[5];
        g := matrix.F[6];
        h := matrix.F[7];
        i := matrix.F[8];

		tempMatrix := newMatrix(
			e*i - f*h, c*h - b*i, b*f - c*e,
			f*g - d*i, a*i - c*g, c*d - a*f,
			d*h - e*g, b*g - a*h, a*e - b*d
		);

		Result := MatrixMultiplyScalar(1.0/det, tempMatrix);

		tempMatrix.free();
	end;

    { https://en.wikipedia.org/wiki/Atan2 }
    function atan2(y, x: float): float;
    begin
        Result := 0.0;
        If (y <> 0.0) then begin
            Result := sqrt(x * x + y * y) - x;
            Result := Result / y;
            Result := atanDeg(Result) * 2.0;
        end else begin
            If (x = 0.0) then begin
                Result := 0;
                exit;
            end;
            Result := atanDeg(y / x);
            If (x < 0) then begin
                Result :=  Result + 180;
            end;
        end;
    end;

    { Converts an axis-angle orientation to Euler angles in degrees. Tailored for Skyrim (extrinsic left-handed ZYX Euler). }
    Function AxisAngleToEuler(afAxisAngle: TJsonObject): TJsonObject;
    var
        matrix: TJsonArray;
    begin
        matrix := AxisAngleToMatrix(afAxisAngle);
        Result := MatrixToEuler(matrix);
        matrix.free();
    end;

    Function QuaternionToEuler(aqQuat: TJsonObject): TJsonObject;
    var
        matrix: TJsonArray;
    begin
        //
        matrix := QuaternionToMatrix(aqQuat);

        Result := MatrixToEuler(matrix);

        matrix.free();
    end;

    { Returns as a new quaternion the Hamilton product of two quaternions (of the form [w, x, y, z]). }
    Function QuaternionMultiply(aqA, aqB: TJsonObject): TJsonObject;
	begin
        {
        float[] qOut = new float[4]
        qOut[0] = aqA[0] * aqB[0] - aqA[1] * aqB[1] - aqA[2] * aqB[2] - aqA[3] * aqB[3]
        qOut[1] = aqA[0] * aqB[1] + aqA[1] * aqB[0] + aqA[2] * aqB[3] - aqA[3] * aqB[2]
        qOut[2] = aqA[0] * aqB[2] - aqA[1] * aqB[3] + aqA[2] * aqB[0] + aqA[3] * aqB[1]
        qOut[3] = aqA[0] * aqB[3] + aqA[1] * aqB[2] - aqA[2] * aqB[1] + aqA[3] * aqB[0]
        return qOut
        }
        Result := newQuaternion(
            aqA.F['w'] * aqB.F['w'] - aqA.F['x'] * aqB.F['x'] - aqA.F['y'] * aqB.F['y'] - aqA.F['z'] * aqB.F['z'],
            aqA.F['w'] * aqB.F['x'] + aqA.F['x'] * aqB.F['w'] + aqA.F['y'] * aqB.F['z'] - aqA.F['z'] * aqB.F['y'],
            aqA.F['w'] * aqB.F['y'] - aqA.F['x'] * aqB.F['z'] + aqA.F['y'] * aqB.F['w'] + aqA.F['z'] * aqB.F['x'],
            aqA.F['w'] * aqB.F['z'] + aqA.F['x'] * aqB.F['y'] - aqA.F['y'] * aqB.F['x'] + aqA.F['z'] * aqB.F['w']
        );
    end;

    { Converts a set of Euler angles to a quaternion (represented as [w, x, y, z]). Tailored for Skyrim (extrinsic left-handed ZYX Euler). }
    function EulerToQuaternion(afX, afY, afZ: float): TJsonObject;
    var
        axisAngle: TJsonObject;
    begin
        axisAngle := EulerToAxisAngle(afX, afY, afZ);
        Result := AxisAngleToQuaternion(axisAngle);
        axisAngle.free();
    end;

    {
        Given two sets of positions and rotations -- those of a parent object, and those of a child object relative to the parent -- this function
        returns a TJsonObject with the keys 'pos' and 'rot', each of which contains the keys 'x', 'y', and 'z'.
        These are the positions and rotations of the child object relative to the world.
        In other words, this function exists as an alternative to MoveObjectRelativeToObject, allowing you to move objects however you wish.

        Position code was inspired by GetPosXYZRotateAroundRef, a function authored by Chesko that can be found on the Creation Kit wiki.
    }
    function GetCoordinatesRelativeToBase(afParentPosition, afParentRotation, afOffsetPosition, afOffsetRotation: TJsonObject): TJsonObject;
    var
        mParentRotation, matrixParent, matrixChild, matrixDone: TJsonArray;
        vChildPosition, qParent, qChild, qDone: TJsonObject;
    begin
        Result := TJsonObject.create;

        mParentRotation := EulerToMatrix(afParentRotation.F['x'], afParentRotation.F['y'], afParentRotation.F['z']);
        vChildPosition := MatrixMultiplyByColumn(mParentRotation, afOffsetPosition);

        Result.O['pos'] := VectorAdd(vChildPosition, afParentPosition);

        {
        qParent := EulerToQuaternion(afParentRotation.F['x'], afParentRotation.F['y'], afParentRotation.F['z']);
        AddMessage('qParent '+qParent.toJSON());
        qChild := EulerToQuaternion(afOffsetRotation.F['x'], afOffsetRotation.F['y'], afOffsetRotation.F['z']);
        AddMessage('qChild '+qChild.toJSON());
        qDone := QuaternionMultiply(qParent, qChild);
        }

        // matrixParent := EulerToMatrix(afParentRotation.F['x'], afParentRotation.F['y'], afParentRotation.F['z']);
        matrixChild  := EulerToMatrix(afOffsetRotation.F['x'], afOffsetRotation.F['y'], afOffsetRotation.F['z']);
        matrixDone   := MatrixMultiply(mParentRotation, matrixChild);


        Result.O['rot'] := MatrixToEuler(matrixDone);//QuaternionToEuler(qDone);

        mParentRotation.free();
        vChildPosition.free();
        //qParent.free();
        //qChild.free();
        //qDone.free();

        //matrixParent.free();
        matrixChild.free();
        matrixDone.free();
    end;

	{
		Inverse of GetCoordinatesRelativeToBase:
		Takes two sets of absolute positions/rotations -- those of a parent and an intended child -- and calculates the position/rotation of the child relative to the parent.
		Return value is as above: TJsonObject containing pos and rot, each of which contains x, y, z

		Warning: this might return nil, if it fails to invert the rotational matrix. I *think* this shouldn't ever happen, but I don't know what it would mean if it does.
	}
	function ConvertAbsoluteCoordinatesToBaseRelative(afParentPosition, afParentRotation, afOffsetPosition, afOffsetRotation: TJsonObject): TJsonObject;
	var
		parentMatrix, parentMatrixInverse, matrixChild, matrixChildRotated, vChildPos, rotWhat: TJsonObject;
	begin
		parentMatrix := EulerToMatrix(afParentRotation.F['x'], afParentRotation.F['y'], afParentRotation.F['z']);
		parentMatrixInverse := InvertMatrix(parentMatrix);
		if(parentMatrixInverse = nil) then begin
			AddMessage('ERROR: ConvertAbsoluteCoordinatesToBaseRelative failed, matrix is not invertible');
			Result := nil;
			parentMatrix.free();
			parentMatrixInverse.free();
			exit;
		end;

		Result := TJsonObject.create;

		matrixChild  := EulerToMatrix(afOffsetRotation.F['x'], afOffsetRotation.F['y'], afOffsetRotation.F['z']);

		// undo matrixParent * matrixChild
		rotWhat := MatrixMultiply(parentMatrixInverse, matrixChild);

		Result.O['rot'] := MatrixToEuler(rotWhat);

		// now undo the positional offsetting
		vChildPos := VectorSubtract(afOffsetPosition, afParentPosition);

		// and now unrotate the vChildPos
		Result.O['pos'] := MatrixMultiplyByColumn(parentMatrixInverse, vChildPos);

		rotWhat.free();
		parentMatrix.free();
		parentMatrixInverse.free();
		matrixChild.free();
		vChildPos.free();
	end;

    { Moves the child reference relative to the parent reference. Position code is based on GetPosXYZRotateAroundRef, a function authored by Chesko that can be found on the Creation Kit wiki. }
    Function MoveObjectRelativeToObject(afParentPosition, afParentRotation, afPositionOffset, afRotationOffset: TJsonObject): TJsonObject;
    var
        Angles, Origin, Output, Vector: TJsonObject;
        qParent, qChild, qDone, eDone: TJsonObject;

    begin
        // Function MoveObjectRelativeToObject(ObjectReference akChild, ObjectReference akParent, float[] afPositionOffset, float[] afRotationOffset) global
        Angles := newVector(0,0,0);//afParentRotation;
        Angles.F['x'] := -afParentRotation.F['x'];
        Angles.F['y'] := -afParentRotation.F['y'];
        Angles.F['z'] := -afParentRotation.F['z'];

        Origin := afParentPosition;

        Output := newVector(0, 0, 0);
        Vector := newVector(0, 0, 0);

        // Output[0] = afPositionOffset[0] * Math.cos(Angles[2]) + afPositionOffset[1] * Math.sin(-Angles[2])
        Output.F['x'] := afPositionOffset.F['x'] * cosDeg(Angles.F['z']) + afPositionOffset.F['y'] * sinDeg(-Angles.F['z']);
        // Output[1] = afPositionOffset[0] * Math.sin(Angles[2]) + afPositionOffset[1] * Math.cos(Angles[2])
        Output.F['y'] := afPositionOffset.F['x'] * sinDeg(Angles.F['z']) + afPositionOffset.F['y'] * cosDeg(Angles.F['z']);
        // Output[2] = afPositionOffset[2]
        Output.F['z'] := afPositionOffset.F['z'];

        // Vector[0] = Output[0]
        Vector.F['x'] := Output.F['x'];
        // Vector[2] = Output[2]
        Vector.F['z'] := Output.F['z'];
        // Output[0] = Vector[0] * Math.cos(Angles[1]) + Vector[2] * Math.sin(Angles[1])
        Output.F['x'] := Vector.F['x'] * cosDeg(Angles.F['y']) + Vector.F['z'] * sinDeg(Angles.F['y']);
        //Output[2] = Vector[0] * Math.sin(-Angles[1]) + Vector[2] * Math.cos(Angles[1])
        Output.F['z'] := Vector.F['x'] * sinDeg(-Angles.F['y']) + Vector.F['z'] * cosDeg(Angles.F['y']);

        // Vector[1] = Output[1]
        Vector.F['y'] := Output.F['y'];
        // Vector[2] = Output[2]
        Vector.F['z'] := Output.F['z'];

        // Output[1] = Vector[1] * Math.cos(Angles[0]) + Vector[2] * Math.sin(-Angles[0])
        Output.F['y'] := Vector.F['y'] * cosDeg(Angles.F['x']) + Vector.F['z'] * sinDeg(-Angles.F['x']);
        // Output[2] = Vector[1] * Math.sin(Angles[0]) + Vector[2] * Math.cos(Angles[0])
        Output.F['z'] := Vector.F['y'] * sinDeg(Angles.F['x']) + Vector.F['z'] * cosDeg(Angles.F['x']);

        // Output[0] = Output[0] + Origin[0]
        // Output[1] = Output[1] + Origin[1]
        // Output[2] = Output[2] + Origin[2]
        Output.F['x'] := Output.F['x'] + Origin.F['x'];
        Output.F['y'] := Output.F['y'] + Origin.F['y'];
        Output.F['z'] := Output.F['z'] + Origin.F['z'];

        // float[] qParent = autobuilder:cobblibraryrotations.EulerToQuaternion(akParent.GetAngleX(), akParent.GetAngleY(), akParent.GetAngleZ())
        qParent := EulerToQuaternion(afParentPosition.F['x'], afParentPosition.F['y'], afParentPosition.F['z']);
        // float[] qChild = autobuilder:cobblibraryrotations.EulerToQuaternion(afRotationOffset[0], afRotationOffset[1], afRotationOffset[2])
        qChild := EulerToQuaternion(afRotationOffset.F['x'], afRotationOffset.F['y'], afRotationOffset.F['z']);

        // float[] qDone = autobuilder:cobblibraryrotations.QuaternionMultiply(qParent, qChild)
        qDone := QuaternionMultiply(qParent, qChild);
        //float[] eDone = autobuilder:cobblibraryrotations.QuaternionToEuler(qDone)
        eDone := QuaternionToEuler(qDone);

        // akChild.SetPosition(Output[0], Output[1], Output[2])
        // akChild.SetAngle(eDone[0], eDone[1], eDone[2])
        Result := TJsonObject.create;
        Result.O['pos'] := Output;
        Result.O['rot'] := eDone;

         {
        Origin.free();
        Angles.free();
        qDone.free();
        Vector.free();
        qParent.free();
        qChild.free();
        }
    end;

    Function MatrixToQuaternion(afMatrix: TJsonArray): TJsonObject;
    var
        axisAngle: TJsonArray;
    begin
        axisAngle := MatrixToAxisAngle(afMatrix);
        Reuslt := AxisAngleToQuaternion(axisAngle);
        axisAngle.free();
    end;

    { Converts a rotation matrix to Euler angles. Tailored for Skyrim (extrinsic left-handed ZYX Euler). }
    Function MatrixToEuler(afMatrix: TJsonArray): TJsonObject;
    var
        fY, fCY, fCYTest, fTX, fTY: float;
    begin
        Result := newVector(0,0,0);

        fY := asinDeg(-1.0 * trunc(afMatrix.F[2] * 1000000.0) / 1000000.0);
        fCY := cosDeg(fY);
        fCYTest := trunc(fCY * 100.0) / 100.0;
        fTX := 0;
        fTY := 0;
        If (fCY <> 0) and (fCY >= 0.00000011920929) and (fCYTest <> 0) then begin
            fTX := afMatrix.F[8] / fCY;
            fTY := afMatrix.F[5] / fCY;
            Result.F['x'] := atan2(fTY, fTX);
            fTX := afMatrix.F[0] / fCY;
            fTY := afMatrix.F[1] / fCY;
            Result.F['z'] := atan2(fTY, fTX);
        end Else begin
            Result.F['x'] := 0.0;
            fTX := afMatrix.F[4];
            fTY := afMatrix.F[3];
            Result.F['z'] := -1.0 * atan2(fTY, fTX);
        end;
        Result.F['y'] := fY;
    end;

    {Converts an axis-angle orientation to a rotation matrix. Tailored for Skyrim (extrinsic left-handed ZYX Euler).}
    function AxisAngleToMatrix(afAxisAngle: TJsonObject): TJsonArray;
    var
        fOneMinusCos, angleSin, angleCos: float;
    begin
        {
        Based on the math at: https://en.wikipedia.org/wiki/Rotation_matrix#Rotation_matrix_from_axis_and_angle

        The source does NOT state its Euler sequence, and it isn't entirely clear about its handedness
        or whether or not it's extrinsic, either. Proceed with caution. It DOES line up with the other
        sites I've been using, though.
        }
        angleSin := sinDeg(afAxisAngle.F['angle']);
        angleCos := cosDeg(afAxisAngle.F['angle']);
        fOneMinusCos := (1 - angleCos);

        Result := newMatrix(
            angleCos + sqr(afAxisAngle.F['x']) * fOneMinusCos,
            afAxisAngle.F['x'] * afAxisAngle.F['y'] * fOneMinusCos - afAxisAngle.F['z'] * angleSin),
            afAxisAngle.F['x'] * afAxisAngle.F['z'] * fOneMinusCos + afAxisAngle.F['y'] * angleSin),
            afAxisAngle.F['y'] * afAxisAngle.F['x'] * fOneMinusCos + afAxisAngle.F['z'] * angleSin),
            angleCos + sqr(afAxisAngle.F['y']) * fOneMinusCos,
            afAxisAngle.F['y'] * afAxisAngle.F['z'] * fOneMinusCos - afAxisAngle.F['x'] * angleSin),
            afAxisAngle.F['z'] * afAxisAngle.F['x'] * fOneMinusCos - afAxisAngle.F['y'] * angleSin),
            afAxisAngle.F['z'] * afAxisAngle.F['y'] * fOneMinusCos + afAxisAngle.F['x'] * angleSin),
            angleCos + sqr(afAxisAngle.F['z']) * fOneMinusCos
        );

        {
    float[] fMatrix = new float[9]
	float fOneMinusCos = 1 as float - Math.cos(afAxisAngle[3])
	fMatrix[0] = Math.cos(afAxisAngle[3]) + Math.pow(afAxisAngle[0], 2 as float) * fOneMinusCos
	fMatrix[1] = afAxisAngle[0] * afAxisAngle[1] * fOneMinusCos - afAxisAngle[2] * Math.sin(afAxisAngle[3])
	fMatrix[2] = afAxisAngle[0] * afAxisAngle[2] * fOneMinusCos + afAxisAngle[1] * Math.sin(afAxisAngle[3])
	fMatrix[3] = afAxisAngle[1] * afAxisAngle[0] * fOneMinusCos + afAxisAngle[2] * Math.sin(afAxisAngle[3])
	fMatrix[4] = Math.cos(afAxisAngle[3]) + Math.pow(afAxisAngle[1], 2 as float) * fOneMinusCos
	fMatrix[5] = afAxisAngle[1] * afAxisAngle[2] * fOneMinusCos - afAxisAngle[0] * Math.sin(afAxisAngle[3])
	fMatrix[6] = afAxisAngle[2] * afAxisAngle[0] * fOneMinusCos - afAxisAngle[1] * Math.sin(afAxisAngle[3])
	fMatrix[7] = afAxisAngle[2] * afAxisAngle[1] * fOneMinusCos + afAxisAngle[0] * Math.sin(afAxisAngle[3])
	fMatrix[8] = Math.cos(afAxisAngle[3]) + Math.pow(afAxisAngle[2], 2 as float) * fOneMinusCos
	return fMatrix}

    end;

    { UNTESTED. Returns as a new quaternion the conjugate of the given quaternion (of the form [w, x, y, z]). }
    Function QuaternionConjugate(aq: TJsonObject): TJsonObject;
    var
        v, v2: TJsonObject;
    begin

        v := newVector(0, aq.F['w'], aq.F['x']); // I have no idea what I am doing
        v2 := VectorNegate(v);

        Result := newQuaternion(
            aq.F['w'],
            v2.F['y']
            v2.F['z'],
            0
        );

        v.free();
        v2.free();

    end;

    {Converts a rotation matrix to axis angle, returning [x, y, z, angle]. The angle is in degrees. Tailored for Skyrim (extrinsic left-handed ZYX Euler).}
    Function MatrixToAxisAngle(afMatrix: TJsonArray): TJsonObject;
    var
        fNormalized: TJsonObject;
        fTrace, fTemporary: float;
        iLargestIndex, iIterator, iIndex, iSign: integer;
        vectorComponent: string;
    begin
        Result := newAxisAngle(0, 0, 0, 0);

        // Determine the angle.
        fTrace := MatrixTrace(afMatrix);
        Result.F['angle'] := acosDeg((fTrace - 1.0) / 2.0);

        // Determine the axis.

        Result.F['x'] := afMatrix.F[7] - afMatrix.F[5];
        Result.F['y'] := afMatrix.F[2] - afMatrix.F[6];
        Result.F['z'] := afMatrix.F[3] - afMatrix.F[1];
        If (Result.F['angle'] = 180) then begin

          // A 180-degree angle tends to lead to a zero vector as our axis.
          // There seems to be a way to correct that...
          // Source for the math: http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToAngle/index.htm
          // Source for the math: http://sourceforge.net/p/mjbworld/discussion/122133/thread/912b44f7

          Result.F['x'] := sqrt((afMatrix.F[0] + 1) / 2.0);
          Result.F['y'] := sqrt((afMatrix.F[4] + 1) / 2.0);
          Result.F['z'] := sqrt((afMatrix.F[8] + 1) / 2.0);

          // We don't know the signs of the above terms. Per our second
          // source, we can start to figure that out by finding the largest
          // term, and then...

          iLargestIndex := 0;
          fTemporary := Result.F['x'];
          If (fTemporary < Result.F['y']) then begin
             fTemporary := Result.F['y'];
             iLargestIndex := 1;
          end;

          If (fTemporary < Result.F['z']) then begin
             fTemporary := Result.F['z'];
             iLargestIndex := 2;
          end;

          for iIterator := 0 to 2 do begin
             iIndex := iLargestIndex * 3 + iIterator;
             If (iIterator <> iLargestIndex) then begin
                //
                // Get the sign of the relevant matrix term.
                //
                iSign := 0;
                If (afMatrix.F[iIndex] <> 0) then begin
                   iSign := 1;
                   If (afMatrix.F[iIndex] < 0) then begin
                      iSign := -1;
                   end;
                end;

                // Result.
                vectorComponent := indexToVectorComponent(iIterator);
                Result.F[vectorComponent] := Result.F[vectorComponent] * iSign;
             end;
          end;
        end; // of angle being 180

        // Normalize the axis.

        If (VectorLength(Result) <> 0) then begin
          fNormalized := VectorNormalize(Result);
          Result.F['x'] := fNormalized.F['x'];
          Result.F['y'] := fNormalized.F['y'];
          Result.F['z'] := fNormalized.F['z'];
          fNormalized.free();
        end Else begin
          // Edge-case caused a zero vector! Dumb fallback to the Z-axis.
          Result.F['x'] := 0;
          Result.F['y'] := 0;
          Result.F['z'] := 1;
        end;

    end;

    // Xedit-specific utility stuff
    function getPositionVector(e: IInterface; path: string): TJsonObject;
    begin
        if(path <> '') then path := path + '\';

        Result := newVector(
            StrToFloat(GetElementEditValues(e, path+'Position\X')),
            StrToFloat(GetElementEditValues(e, path+'Position\Y')),
            StrToFloat(GetElementEditValues(e, path+'Position\Z'))
        );
    end;

    function getRotationVector(e: IInterface; path: string): TJsonObject;
    begin
        if(path <> '') then path := path + '\';
        Result := newVector(
            StrToFloat(GetElementEditValues(e, path+'Rotation\X')),
            StrToFloat(GetElementEditValues(e, path+'Rotation\Y')),
            StrToFloat(GetElementEditValues(e, path+'Rotation\Z'))
        );
    end;


end.