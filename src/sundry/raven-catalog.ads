--  SPDX-License-Identifier: ISC
--  Reference: /License.txt


package Raven.Catalog is

   --  Looks for catalog.ucl at ${RVN_CACHEDIR}/catalog/catalog.ucl
   --  If ${RVN_DBDIR}/catalog.sqlite exists, rename to ${RVN_DBDIR}/catalog.sqlite.backup
   --  Generate empty database at ${RVN_DBDIR}/catalog.sqlite
   --  Read catalog.ucl line by line:
   --       - parse line into UCL
   --       - convert to pkg structure
   --       - insert pkg data into catalog.sqlite (atomic transaction)
   --  upon success, delete ${RVN_DBDIR}/catalog.sqlite.back and return True
   --  otherwise:
   --       - delete ${RVN_DBDIR}/catalog.sqlite
   --       - rename ${RVN_DBDIR}/catalog.sqlite.backup to ${RVN_DBDIR}/catalog.sqlite
   --       - return False

   function generate_database (tracker : out Natural) return Boolean;


   --  To be run after downloading a new catalog
   --  This procedure removes links to cached packages that are obsolete (which is
   --  defined as not matching the checksums in the latest catalog)
   --  No packages are deleted though.
   procedure remove_obsolete_cache_links;

end Raven.Catalog;
