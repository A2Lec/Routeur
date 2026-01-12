with LCA;
with Adresse_IP;  use Adresse_IP;
with Cache_Liste;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Routeur_Cache_Liste is

    type T_Route is record
       Masque : T_IP;
       Inter  : Unbounded_String;
    end record;

    package Tab_Routage is new LCA (T_Cle => T_IP, T_Valeur => T_Route);
    use Tab_Routage;

	

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
        Table_Routage : in Tab_Routage.T_LCA;
	Taille : Integer
    );


end Routeur_Cache_Liste;