DROP DATABASE IF EXISTS adalo;
CREATE DATABASE adalo;

-- need the base intersects table with all possible events - will insert the values from R client (see /RefugePrioritization/PhaseII/Warehouse/populateWarehouseTables.R)
USE adalo;
DROP TABLE IF EXISTS baseIntersects;
CREATE TABLE baseIntersects (
  intId MEDIUMINT UNSIGNED NOT NULL PRIMARY KEY,
  padusObjId MEDIUMINT UNSIGNED NOT NULL,
  USFWSregion TINYINT(1) UNSIGNED NOT NULL,
  USFSregion TINYINT(1) UNSIGNED NOT NULL,
  NPSregion TINYINT(1) UNSIGNED NOT NULL,
  LCCregion TINYINT(2) UNSIGNED NOT NULL,
  USJVregion TINYINT(2) UNSIGNED NOT NULL,
  BCRregion TINYINT(2) UNSIGNED NOT NULL,
  StateFIPS CHAR(2) NOT NULL,
  CountyFIPS CHAR(5) NOT NULL,
  g990cellId MEDIUMINT UNSIGNED NOT NULL,
  ncells SMALLINT UNSIGNED NOT NULL,
  KEY regions (USFWSregion,USFSregion,NPSregion,LCCregion,USJVregion,BCRregion),
  KEY fws (USFWSregion),
  KEY padus (padusObjId)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;


 -- need a lookup table for padusObjId - will insert the values from R client (see /RefugePrioritization/PhaseII/Warehouse/populateWarehouseTables.R)
USE adalo;
DROP TABLE IF EXISTS padusCats;
CREATE TABLE padusCats (
  padusObjId MEDIUMINT UNSIGNED NOT NULL PRIMARY KEY,
  mgmtType CHAR(4),
  mgrName CHAR(5),
  desType CHAR(45),
  unitName CHAR(120),
  KEY units (padusObjId,unitName),
  KEY mangtp (padusObjId,mgmtType),
  KEY mangnm (padusObjId,mgrName),
  KEY destp (padusObjId,desType)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- need lookup tables for mgmtType, mgrName, and desType - can all be in 1 table
USE adalo;
DROP TABLE IF EXISTS padusCatsLookup;
CREATE TABLE padusCatsLookup (
  padusCat VARCHAR(8) NOT NULL,
  categoryCode VARCHAR(5) NOT NULL,
  codeDescription VARCHAR(40),
  KEY catLook (padusCat, categoryCode)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;


-- need a lookup table of species
USE adalo;
DROP TABLE IF EXISTS species;
CREATE TABLE species (
  speciesCode CHAR(4) NOT NULL PRIMARY KEY,
  speciesName VARCHAR(30) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

INSERT INTO species VALUES ('BAIS','Baird''s Sparrow'),('BLRA','Black Rail'),('BOBO','Bobolink'),('BUOW','Burrowing Owl'),('CANV','Canvasback'),
    ('CCLO','Chestnut-collared Longspur'),('FEHA','Ferruginous Hawk'),('LBCU','Long-billed Curlew'),('LETE','Least tern'),('MAGO','Marbled Godwit'),
    ('MOPL','Mountain Plover'),('NOPI','Northern Pintail'),('RIRA','Ridgway''s Rail'),('SACR','Sandhill Crane'),('SNPL','Snowy Plover'),
    ('SPPI','Sprague''s Pipit'),('TRBL','Tri-colored Blackbird'),('WIFL','Southwestern Willow Flycatcher');


-- need a table per species, so creating baseSpecies and copying to all species in stored procedure below
USE adalo;
DROP TABLE IF EXISTS baseSpecies;
CREATE TABLE baseSpecies (
  intId MEDIUMINT UNSIGNED NOT NULL,
  padusObjId MEDIUMINT UNSIGNED NOT NULL,
  ncells SMALLINT UNSIGNED NOT NULL,
  period TINYINT(1) UNSIGNED NOT NULL,
  metric TINYINT(1) UNSIGNED NOT NULL,
  metricValue FLOAT UNSIGNED NOT NULL,
  cellMetric FLOAT UNSIGNED NOT NULL,
  PRIMARY KEY intKey (intId,period,metric),
  KEY padus (padusObjId)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;



USE adalo;
DROP PROCEDURE IF EXISTS ROWPERROW;
DELIMITER ;;

CREATE PROCEDURE ROWPERROW()
BEGIN
DECLARE n INT DEFAULT 0;
DECLARE i INT DEFAULT 0;
DECLARE s CHAR(4) DEFAULT 'XXXX';
SELECT COUNT(*) FROM species INTO n;
SET i=0;
WHILE i<n DO 
  SET @s = (SELECT speciesCode FROM species LIMIT i,1);
  SET @c = CONCAT('DROP TABLE IF EXISTS ',@s);
  PREPARE stmt from @c;
  EXECUTE stmt;
  DEALLOCATE PREPARE stmt;
  SET @c = CONCAT('CREATE TABLE ',@s, ' LIKE `baseSpecies`');
  PREPARE stmt from @c;
  EXECUTE stmt;
  DEALLOCATE PREPARE stmt;
  SET i = i + 1;
END WHILE;
END;
;;

DELIMITER ;

CALL ROWPERROW();

-- Individual species observations populated from R client (see /RefugePrioritization/PhaseII/Warehouse/populateWarehouseTables.R)

