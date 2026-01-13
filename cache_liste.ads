with LCA;
with Adresse_IP;  use Adresse_IP;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package Cache_Liste is

    type T_Route is record
       Masque : T_IP;
       Inter  : Unbounded_String;
    end record;

    package Tab_Routage is new LCA (T_Cle => T_IP, T_Valeur => T_Route);
    use Tab_Routage;

    type T_LCA_Access is access constant T_LCA;

    type T_Cache_Liste (Place : Integer) is limited private;

    type T_Politique is (FIFO, LRU, LFU);

    type T_Destination is record
        Adresse : T_IP;
        Occurrences : Integer;
    end record;

    type T_Ordre_Politique (Place : Integer) is private;

    type T_Statistiques is record
        Nombre_Routes : Float;
        Nombre_Defauts : Float;
    end record;



    procedure Afficher_Table is new Tab_Routage.Faire_Pour_Chaque (Traiter => Afficher_Une_Route);


    procedure Afficher_Une_Route (Destination : in T_IP; Route : in T_Route);

    procedure Afficher_Table_Cache (Cache : in T_Cache_Liste);

    procedure Afficher_Table (Liste : in T_LCA);


    procedure Creer (Cache : out T_Cache_Liste; Politique : in T_Politique);

    procedure Supprimer (Cache : in out T_Cache_Liste);

    procedure Detruire (Cache : in out T_Cache_Liste);

    procedure Ajouter (Cache : in out T_Cache_Liste; Adresse : in T_IP; Interface_S : Unbounded_String; Bonne_Interface_S : Unbounded_String);

    function Stats (Cache : T_Cache_Liste) return T_Statistiques;

    procedure Afficher_Table (Cache : in T_Cache_Liste);

private

    type T_Tab_Adresses is Array (Positive range <>) of T_Destination;

    type T_Ordre_Politique (Place : Integer) is record
        Adresses : T_Tab_Adresses(1..Place);
        Taille : Integer;
    end record;

    type T_Cache_Liste (Place : Integer) is record
        Capacite : Integer;
        Contenu : T_LCA;
        Politique : T_Politique;
        Ordre_Politique : T_Ordre_Politique(Place);
        Statistiques : T_Statistiques;
    end record;

end Cache_Liste;
