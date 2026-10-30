SELECT * FROM OCRD AS "Clientes"
select * FROM OSLP AS "Vendedores"

select * FROM OINV AS "Facturas"
select * FROM INV1 AS "Detalle_Facturas"
select * FROM ORIN AS "Notas_Credito"
select * FROM RIN1 AS "Notas_Credito_Detalle"

select * FROM OCTG AS "Tipo_Credito"
select * FROM ORTT AS "Tipo_Cambio"
select * FROM OITM AS "Productos"
select * FROM OITB AS "Grupo"
select * FROM OITT AS "Materiales"
select * FROM DPI1 AS "KIT"

select * FROM OITR AS "Reconciliaciones"
select * FROM ITR1 AS "Detalle_Reconciliaciones"

select * FROM AITM AS "Modificaciones_Productos"

SELECT * FROM OPCH AS "Facturas_Proveedores"
SELECT * FROM PCH1 AS "Detalle_Facturas_Proveedores"
SELECT * FROM ORPC AS "Notas_Creditos_Proveedores"

SELECT * FROM OPDN AS "Entradas_Mercancias"
SELECT * FROM OPOR AS "Ordenes_Compra"

SELECT * FROM OWTR AS "Trasferencias de Stocks"
SELECT * FROM WTR1 AS "Detalle Trasferencias de Stocks"

SELECT * FROM OJDT as "CuentasContables"
SELECT * FROM JDT1 as "CuentasContablesDetalle"




SELECT 
    "COLUMN_NAME"
FROM "SYS"."TABLE_COLUMNS"
WHERE "SCHEMA_NAME" = 'SBO_PERU_PROD'
  AND "TABLE_NAME" = 'OPCH';


SELECT
    "SCHEMA_NAME",
    "TABLE_NAME",
    "COLUMN_NAME"
FROM "SYS"."TABLE_COLUMNS"
WHERE UPPER("COLUMN_NAME") LIKE '%INDICATOR%' AND "SCHEMA_NAME" = 'SBO_RUIGU_PROD';






--RECONCILIACIONES
SELECT 'PERU' AS "EMPRESA",
    T1."ReconNum",
    T3."DocDate" AS "FECHA_EMISION",
    T5."ExtraDays" AS "DIAS_CREDITO",
    T3."DocDueDate" AS "FECHA_VENCIMIENTO",
    T2."ReconDate" AS "FECHA_PAGO",
    T3."DocCur" AS "MONEDA",
    DAYS_BETWEEN(T3."DocDueDate", T2."ReconDate") AS "TIEMPO",
	T7."PymntGroup",
    T3."NumAtCard",
    CASE  
        WHEN LEFT(T3."NumAtCard", 4) IN ('F001','F101','B001','B101') THEN 'LIMA'
        WHEN LEFT(T3."NumAtCard", 4) IN ('F002','F102','B002','B102') THEN 'AREQUIPA'
        WHEN LEFT(T3."NumAtCard", 4) IN ('F005','F105','B005','B105') THEN 'PIURA'
        WHEN LEFT(T3."NumAtCard", 4) IN ('F007','F107','B007','B107') THEN 'MILLER'
        WHEN LEFT(T3."NumAtCard", 4) IN ('F004','F104','B004','B104') THEN 'CHIMBOTE'
        WHEN LEFT(T3."NumAtCard", 4) IN ('F006','F106','B006','B106') THEN 'TRUJILLO' 
    END AS "SUCURSAL",
    CAST(T1."ReconSumSC" AS FLOAT) AS "MONTO_USD",
    T3."CardName",
    T3."DocTotal",
    T1."ReconSum",
    T1."ReconSumFC",
    T3."DpmAmnt",
    T3."DpmAmntSC",
    T6."SlpName"
FROM "SBO_PERU_PROD"."ITR1" T1
JOIN "SBO_PERU_PROD"."OITR" T2 ON T1."ReconNum" = T2."ReconNum"
JOIN "SBO_PERU_PROD"."OINV" T3 ON T3."DocEntry" = T1."SrcObjAbs"
JOIN "SBO_PERU_PROD"."OCTG" T5 ON T5."GroupNum" = T3."GroupNum"
JOIN "SBO_PERU_PROD"."OSLP" T6 ON T3."SlpCode" = T6."SlpCode"
JOIN "SBO_PERU_PROD"."OCTG" T7 ON T3."GroupNum"= T7."GroupNum"
WHERE T2."ReconDate" >= '2026-09-01'
  	AND T2."ReconDate" <= '2026-12-31'
    AND T2."Canceled" = 'N'
    AND T3.Canceled = 'N'
    AND T1."SrcObjTyp" = '13'
    AND T1."ReconSum" <> 0
    AND T1."ReconSum" > -1
    AND T3."CardCode" NOT IN ('C20517413233','C20506005133','C20602699120')
ORDER BY T2."ReconDate" DESC



--VENTAS
SELECT
	ODOC."U_DocEntry" AS "DocEntry",
	ODOC."U_DocNum" AS "DocNum",
	ODOC."U_DocDate" AS "DocDate",
	ODOC."U_DocDueDate" AS "DocDueDate",
	ODOC."U_NumAtCard" AS "NumAtCard",
	ODOC."U_LicTradNum" AS "LicTradNum",
	ODOC."U_CardName" AS "CardName",
	ODOC."U_DocCur" AS "DocCur",
	CAST(ODOC."U_DocTotal" AS FLOAT) AS "DocTotal",
	CAST(ODOC."U_DocTotalLC" AS FLOAT) AS "DocTotalLC",
	CAST(ODOC."U_DocTotalFC" AS FLOAT) AS "DocTotalFC",
	CAST(ODOC."U_DpmAmnt" AS FLOAT) AS "DpmAmnt",
	CAST(ODOC."U_DpmAmntFC" AS FLOAT) AS "DpmAmntFC",
	CAST(ODOC."U_DpmAmntLC" AS FLOAT) AS "DpmAmntLC",
	ODOC."U_SlpName" AS "SlpName",
	ODOC."U_OCRD_SlpName" "OCRD_SlpName",
	ODOC."U_OSLP_SUCURSAL" "OSLP_SUCURSAL",
	ODOC."U_PymntGroup" AS "PymntGroup",
	ODOC."U_DGF_FORMAPAGO" AS "U_DGF_FORMAPAGO",
	ODOC."U_QuotationRef",
	ODOC."U_DeliveryRef",
	ODOC."U_SaleOrderRef",
	ODOC."Canceled" AS "CANCELED",
	ODOC."U_B2" AS "U_B2",
	ODOC."U_Created" AS "CreateDate",
	DOC1."U_LineNum" AS "LineNum",
	CAST(DOC1."U_Quantity" AS FLOAT) AS "Quantity",
	CAST(DOC1."U_Price" AS FLOAT) AS "Price",
	CAST(DOC1."U_PriceFC" AS FLOAT) AS "PriceFC",
	CAST(DOC1."U_PriceLC" AS FLOAT) AS "PriceLC",
	CAST(DOC1."U_LineTotal" AS FLOAT) AS "LineTotal",
	CAST(DOC1."U_LineTotalFC" AS FLOAT) AS "LineTotalFC",
	CAST(DOC1."U_LineTotalLC" AS FLOAT) AS "LineTotalLC",
	DOC1."U_ItemCode" AS "ItemCode",
	DOC1."U_Dscription" AS "Dscription", DOC1."U_Dscription" AS "U_ItemDscription",
	DOC1."U_OITM_ItemName" AS "ItemName",
	DOC1."U_PrintText" AS "U_PrintText",
	DOC1."U_PrintText" AS "LineText",
	DOC1."U_KitNum", DOC1."U_PrintInBilling",
	DOC1."U_ItemModel" AS "U_ItemModel",
	DOC1."U_BrandName" AS "U_BrandName",
	DOC1."U_FamilyName" AS "U_FamilyName",
	DOC1."U_ProcessName" AS "U_ProcessName",
	DOC1."U_GroupName" AS "U_GroupName",
	DOC1."U_ReportGroupName" AS "U_ReportGroupName",
	DOC1."U_UnidadDeNegocio" AS "U_UnidadDeNegocio",
	DOC1."U_OITM_ItemModel" AS "OITM_ItemModel",
	DOC1."U_OITM_ItemName" AS "OITM_ItemName",
	DOC1."U_OITM_BrandName" AS "OITM_BrandName",
	DOC1."U_OITM_FamilyName" AS "OITM_FamilyName",
	DOC1."U_OITM_ProcessName" AS "OITM_ProcessName",
	DOC1."U_OITM_GroupName" AS "OITM_GroupName",
	DOC1."U_OITM_ReportGroupName" AS "OITM_ReportGroupName",
	DOC1."U_OITM_UnidadDeNegocio" AS "OITM_UnidadDeNegocio",
	KIT."U_ItemModel" "Kit_ItemModel",
	KIT."U_BrandName" "Kit_BrandName",
	KIT."U_FamilyName" "Kit_FamilyName",
	KIT."U_ProcessName" "Kit_ProcessName",
	KIT."U_GroupName" "Kit_GroupName",
	KIT."U_ReportGroupName" "Kit_ReportGroupName",
	KIT."U_UnidadDeNegocio" "Kit_UnidadDeNegocio",
	DOC1."U_IsKitDetail" "IsKitDetail", DOC1."U_IsKitDetail" "Implementacion",
	ODOC."U_ObjType" "ObjType",
	ODOC."U_TableCode" AS "TableCode",
	ODOC."U_Indicator" AS "Indicator",
	ODOC."U_SUCURSAL" AS "U_SUCURSAL",
    CASE
    	WHEN ODOC."U_Indicator" = '01' THEN 'Factura'
    	WHEN ODOC."U_Indicator" = '03' THEN 'Boleta'
    	WHEN ODOC."U_Indicator" = '07' THEN 'Nota de Credito'
    	WHEN ODOC."U_Indicator" = '08' THEN 'Nota de Debito'
    	ELSE ''
    END AS "Documento",
    ODOC."U_Company" AS "Empresa",
    CAST(ODOC."U_DGF_MONTOPAGOINICIAL" AS FLOAT) AS "U_DGF_MONTOPAGOINICIAL",
    CAST(ODOC."U_DGF_MONTOPAGOINICIAL_FC" AS FLOAT) AS "U_DGF_MONTOPAGOINICIAL_FC",
    CAST(ODOC."U_DGF_MONTOPAGOINICIAL_LC" AS FLOAT) AS "U_DGF_MONTOPAGOINICIAL_LC",
    CAST(DOC1."U_StockPrice" AS FLOAT) AS "StockPrice",
    CAST(DOC1."U_StockPriceFC" AS FLOAT) AS "StockPriceFC",
    CAST(DOC1."U_StockPriceLC" AS FLOAT) AS "StockPriceLC",
    CAST(-DOC1."U_StockValue" AS FLOAT) AS "StockValue",
    CAST(-DOC1."U_StockValueFC" AS FLOAT) AS "StockValueFC",
    CAST(-DOC1."U_StockValueLC" AS FLOAT) AS "StockValueLC",
    CAST(DOC1."U_LineTotalFC" - DOC1."U_StockValueFC" AS FLOAT) AS "UtilidadFC",
    CAST(DOC1."U_LineTotalLC" - DOC1."U_StockValueLC" AS FLOAT) AS "UtilidadLC",
    CAST(ROUND(CASE
    	WHEN DOC1."U_LineTotalFC" != 0 THEN (DOC1."U_LineTotalFC" - DOC1."U_StockValueFC") / DOC1."U_LineTotalFC"
    	ELSE 0
    	END * ODOC."U_CostMultiplier", 4) AS FLOAT) AS "PorcentajeUtilidad",
    DOC1."U_LineID" AS "LineID"
    FROM "SBO_PERU_PROD"."@ODOC_SALES" AS ODOC
    INNER JOIN "SBO_PERU_PROD"."@DOC1_SALES" AS DOC1 ON ODOC."Code" = DOC1."Code"
    LEFT JOIN "SBO_PERU_PROD"."@DOC1_SALES" AS KIT ON DOC1."U_Kit_UID" = KIT."U_LineID"
	WHERE ODOC."Canceled" = 'N'
		AND COALESCE(DOC1."U_HideInSalesReport", 'N') = 'N'
		AND ODOC."U_DocDate" >= '2026-07-01'
		AND ODOC."U_DocDate" <= '2026-12-31'
		AND DOC1."U_ItemCode" <> ''
		AND ODOC."U_LicTradNum" NOT IN ('20517413233','20506005133','20602699120')
		
		
		
		
-- COMPRAS

SELECT
    "OPCH"."DocEntry",
    "OPCH"."DocNum",
    "OPCH"."DocDate" AS "FechaEmision",
    "OPCH"."DocDueDate" AS "FechaVencimiento",
    "OPCH"."NumAtCard" AS "DocumentoProveedor",
    "OPCH"."CardCode" AS "CodigoProveedor",
    "OPCH"."CardName" AS "Proveedor",
    "OPCH"."DocCur" AS "MonedaDocumento",
    "PCH1"."LineNum" AS "NumeroLinea",
    "PCH1"."ItemCode" AS "CodigoArticulo",
    "PCH1"."Dscription" AS "Descripcion",
    "PCH1"."Quantity" AS "Cantidad",
    "PCH1"."LineTotal" AS "TotalLineaML",
    "PCH1"."TotalSumSy" AS "TotalLineaME",
    "OACT"."AcctCode" AS "CodigoCuenta",
    "OACT"."AcctName" AS "Cuenta",
    "CENTRO_COSTO_1"."OcrName" AS "CentroCosto1",
    "CENTRO_COSTO_2"."OcrName" AS "CentroCosto2",
    "CENTRO_COSTO_3"."OcrName" AS "CentroCosto3",
    "CENTRO_COSTO_4"."OcrName" AS "CentroCosto4",
    "CENTRO_COSTO_5"."OcrName" AS "CentroCosto5"
FROM "OPCH"
INNER JOIN "PCH1"
    ON "PCH1"."DocEntry" = "OPCH"."DocEntry"
LEFT JOIN "OACT"
    ON "OACT"."AcctCode" = "PCH1"."AcctCode"
LEFT JOIN "OOCR" AS "CENTRO_COSTO_1"
    ON "CENTRO_COSTO_1"."OcrCode" = "PCH1"."OcrCode" AND "CENTRO_COSTO_1"."DimCode" = 1
LEFT JOIN "OOCR" AS "CENTRO_COSTO_2"
    ON "CENTRO_COSTO_2"."OcrCode" = "PCH1"."OcrCode2" AND "CENTRO_COSTO_2"."DimCode" = 2
LEFT JOIN "OOCR" AS "CENTRO_COSTO_3"
    ON "CENTRO_COSTO_3"."OcrCode" = "PCH1"."OcrCode3" AND "CENTRO_COSTO_3"."DimCode" = 3
LEFT JOIN "OOCR" AS "CENTRO_COSTO_4"
    ON "CENTRO_COSTO_4"."OcrCode" = "PCH1"."OcrCode4" AND "CENTRO_COSTO_4"."DimCode" = 4
LEFT JOIN "OOCR" AS "CENTRO_COSTO_5"
    ON "CENTRO_COSTO_5"."OcrCode" = "PCH1"."OcrCode5" AND "CENTRO_COSTO_5"."DimCode" = 5
WHERE "OPCH"."CANCELED" = 'N'
  AND "OPCH"."DocDate" >= '2026-09-01'
  AND "OPCH"."DocDate" <= '2026-12-31'
ORDER BY "OPCH"."DocDate" DESC, "OPCH"."DocEntry" DESC, "PCH1"."LineNum";



-- CUENTAS CONTABLES
SELECT
'IMPORTACIONES' AS "Empresa",
TD."Account",
TC."BtfStatus",
TC."TransType",
TC."BaseRef",
TC."RefDate",
TC."Memo",
TC."Ref1",
CAST(TC."LocTotal" AS FLOAT) AS "TotalLC",
CAST(TC."SysTotal" AS FLOAT) AS "TotalFC",
CAST(TC."DueDate" AS DATE) AS "DueDate",
TC."TaxDate",
TC."PCAddition",
TC."FinncPriod",
TC."DataSource",
TC."UpdateDate",
CAST(TC."CreateDate" AS DATE) AS "CreateDate",
TC."UserSign",
TC."UserSign2"
FROM "SBO_IMPORTACION_PROD"."OJDT" AS TC LEFT JOIN "SBO_IMPORTACION_PROD"."JDT1" AS TD ON TC."TransId" = TD."TransId"
WHERE TC."CreateDate" >= '2026-01-01' AND TC."CreateDate" <= '2026-12-31'

UNION ALL

SELECT
'PERU' AS "Empresa",
TD."Account",
TC."BtfStatus",
TC."TransType",
TC."BaseRef",
TC."RefDate",
TC."Memo",
TC."Ref1",
CAST(TC."LocTotal" AS FLOAT) AS "TotalLC",
CAST(TC."SysTotal" AS FLOAT) AS "TotalFC",
CAST(TC."DueDate" AS DATE) AS "DueDate",
TC."TaxDate",
TC."PCAddition",
TC."FinncPriod",
TC."DataSource",
TC."UpdateDate",
CAST(TC."CreateDate" AS DATE) AS "CreateDate",
TC."UserSign",
TC."UserSign2"
FROM "SBO_PERU_PROD"."OJDT" AS TC LEFT JOIN "SBO_PERU_PROD"."JDT1" AS TD ON TC."TransId" = TD."TransId"
WHERE TC."CreateDate" >= '2026-01-01' AND TC."CreateDate" <= '2026-12-31'




-- MOVIMIENTOS EN BANCOS

SELECT
	'IMPORTACIONES' AS "Empresa",
    OJDT."TransId" AS "JournalEntry",
    CAST(OJDT."RefDate" AS DATE) AS "Date",
    OJDT."Memo" AS "Comment",
    JDT1."Line_ID" AS "LineNum",
    OACT."FormatCode" AS "AccountCode",
    OACT."AcctName" AS "AccountName",
    CAST(OJDT."LocTotal" AS FLOAT) AS "TotalLC",
	CAST(OJDT."SysTotal" AS FLOAT) AS "TotalFC",
	CAST(JDT1."SYSDeb" AS FLOAT) AS "DebitFC",
	CAST(JDT1."SYSCred" AS FLOAT) AS "CreditFC",
    JDT1."Debit" AS "DebitLC",
    JDT1."Credit" AS "CreditLC"
FROM "SBO_IMPORTACION_PROD"."OJDT" AS OJDT
INNER JOIN "SBO_IMPORTACION_PROD"."JDT1" AS JDT1 ON JDT1."TransId" = OJDT."TransId"
INNER JOIN "SBO_IMPORTACION_PROD"."OACT" AS OACT ON OACT."AcctCode" = JDT1."Account"
WHERE 
	OJDT."RefDate" >= '2026-01-01'
	AND OJDT."RefDate" <= '2026-12-31'
	AND OACT."FormatCode" IN ('10411010000','10411020000','10411030000','10411040000','10411050000','10411060000','10411070000','10411080000')

UNION ALL

SELECT
	'PERU' AS "Empresa",
    OJDT."TransId" AS "JournalEntry",
    CAST(OJDT."RefDate" AS DATE) AS "Date",
    OJDT."Memo" AS "Comment",
    JDT1."Line_ID" AS "LineNum",
    OACT."FormatCode" AS "AccountCode",
    OACT."AcctName" AS "AccountName",
    CAST(OJDT."LocTotal" AS FLOAT) AS "TotalLC",
	CAST(OJDT."SysTotal" AS FLOAT) AS "TotalFC",
	CAST(JDT1."SYSDeb" AS FLOAT) AS "DebitFC",
	CAST(JDT1."SYSCred" AS FLOAT) AS "CreditFC",
    JDT1."Debit" AS "DebitLC",
    JDT1."Credit" AS "CreditLC"
FROM "SBO_PERU_PROD"."OJDT" AS OJDT
INNER JOIN "SBO_PERU_PROD"."JDT1" AS JDT1 ON JDT1."TransId" = OJDT."TransId"
INNER JOIN "SBO_PERU_PROD"."OACT" AS OACT ON OACT."AcctCode" = JDT1."Account"
WHERE 
	OJDT."RefDate" >= '2026-01-01'
	AND OJDT."RefDate" <= '2026-12-31'
	AND OACT."FormatCode" IN ('10411010000','10411020000','10411030000','10411040000','10411050000','10411060000','10411070000','10411080000');
