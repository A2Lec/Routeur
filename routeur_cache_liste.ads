with Cache_Liste;  use Cache_Liste;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Routeur_Cache_Liste is
	

    procedure Recuperer_Arguments (
        Table        : out Unbounded_String;
        Paquets      : out Unbounded_String;
        Resultats    : out Unbounded_String;
        Taille       : out Integer;
        Politique    : out Unbounded_String;
        Statistiques : out Boolean
    );

    procedure Construire_Table (
        Table_Routage    : out Tab_Routage.T_LCA;
        Table            : in Unbounded_String

    );


    procedure Traiter_Fichiers_Paquets (
        Paquets   : in Unbounded_String;
        Resultats : in Unbounded_String;
        Table_Routage : in out Tab_Routage.T_LCA;
        Taille : in Integer;
        Politique : in T_Politique
    );


end Routeur_Cache_Liste;
