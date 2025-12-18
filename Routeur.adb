with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed; 
with Ada.Strings;              use Ada.Strings;      
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

package body Routeur is


    procedure Recuperer_Arguments (
        Table     : out Unbounded_String;
        Paquets   : out Unbounded_String;
        Resultats : out Unbounded_String
    ) is
    begin
       
        Table     := To_Unbounded_String("table.txt");
        Paquets   := To_Unbounded_String("paquets.txt");
        Resultats := To_Unbounded_String("resultats.txt");

       
        for i in 1 .. Argument_Count loop
            if Argument(i) = "-t" and then i < Argument_Count then
                Table := To_Unbounded_String(Argument(i + 1));

            elsif Argument(i) = "-q" and then i < Argument_Count then
                Paquets := To_Unbounded_String(Argument(i + 1)); 

            elsif Argument(i) = "-r" and then i < Argument_Count then
                
                Resultats := To_Unbounded_String(Argument(i + 1));
            end if;
        end loop;
    end Recuperer_Arguments;

    procedure Construire_Table (
        Nom_Fichier : in Unbounded_String;
        La_Table    : out T_Table_Routage
    ) is
        Entree_Table : File_Type;
        Destination  : Unbounded_String;
        Masque       : Unbounded_String;
        Interface_S  : Unbounded_String;
        Line_Content : Unbounded_String;
        Pos1, Pos2   : Integer;
        Trimmed_Line : String(1 .. 1000);
        Trimmed_Len  : Integer;
    begin
        Open(Entree_Table, In_File, To_String(Nom_Fichier));
        

        Tab_Routage.Initialiser(La_Table);

        while not End_Of_File(Entree_Table) loop
            begin
                Line_Content := To_Unbounded_String(Get_Line(Entree_Table));
                Line_Content := To_Unbounded_String(Trim(To_String(Line_Content), Both));
                
                if Length(Line_Content) > 0 and Element(Line_Content, 1) /= '#' then
                    
                    Pos1 := 1;
                    Pos2 := Index(To_String(Line_Content), " ");
                    if Pos2 > 0 then
                        Destination := Unbounded_Slice(Line_Content, Pos1, Pos2 - 1);
                        Pos1 := Pos2 + 1;
                        Pos2 := Index(To_String(Line_Content), " ", Pos1);
                        if Pos2 > 0 then
                            Masque := Unbounded_Slice(Line_Content, Pos1, Pos2 - 1);
                            Interface_S := Unbounded_Slice(Line_Content, Pos2 + 1, Length(Line_Content));
                            Tab_Routage.Enregistrer(La_Table, Destination, Interface_S);
                        end if;
                    end if;
                end if;
            exception
                when End_Error => null; 
            end;
        end loop;

        Close(Entree_Table);
    end Construire_Table;


    procedure Traiter_Ligne (
        Ligne_P          : in Unbounded_String;
        Num_Ligne        : in Integer;
        Sortie_Resultats : in out File_Type;
        Continuer        : in out Boolean;
        La_Table         : in T_Table_Routage
    ) is
        S_Ligne : String := To_String(Ligne_P);
        Interface_Trouvee : Unbounded_String;
        Lookup_Key : Unbounded_String;
        Pipe_Pos : Integer;
        
        procedure Afficher_Cle (Cle : in Unbounded_String) is
        begin
            Put(Sortie_Resultats, To_String(Cle));
        end Afficher_Cle;
        
        procedure Afficher_Donnee (Valeur : in Unbounded_String) is
        begin
            Put(Sortie_Resultats, " -> " & To_String(Valeur));
        end Afficher_Donnee;
        
        procedure Afficher_Tout is new Tab_Routage.Afficher_Diagnostic(Afficher_Cle, Afficher_Donnee);
        
    begin

        S_Ligne := Trim(S_Ligne, Both);

        if S_Ligne = "table" then
            Put(Sortie_Resultats, "table (ligne " & Integer'Image(Num_Ligne) & ")");
            New_Line(Sortie_Resultats);

            Afficher_Tout(La_Table);
            New_Line(Sortie_Resultats);

        elsif S_Ligne = "fin" then
            Continuer := False; 
            Put(Sortie_Resultats, "fin (ligne " & Integer'Image(Num_Ligne) & ")");
            New_Line(Sortie_Resultats);

        elsif S_Ligne'Length > 0 then
            
            Lookup_Key := To_Unbounded_String(S_Ligne);
            Pipe_Pos := Index(S_Ligne, "|");
            if Pipe_Pos > 0 then
                Lookup_Key := To_Unbounded_String(S_Ligne(1 .. Pipe_Pos - 1));
            end if;
            
            if Tab_Routage.Cle_Presente(La_Table, Lookup_Key) then
                Interface_Trouvee := Tab_Routage.La_Valeur(La_Table, Lookup_Key);
                Put(Sortie_Resultats, S_Ligne & " -> " & To_String(Interface_Trouvee)); 
            else
                Put(Sortie_Resultats, S_Ligne & " -> ?");
            end if;
            New_Line(Sortie_Resultats);
        end if;
    end Traiter_Ligne;


    procedure Traiter_Fichiers_Paquets (
        Fichier_Paquets   : in Unbounded_String;
        Fichier_Resultats : in Unbounded_String;
        La_Table          : in T_Table_Routage
    ) is
        Entree_Paquets   : File_Type;
        Sortie_Resultats : File_Type;
        Continuer        : Boolean := True;
        Ligne_P          : Unbounded_String;
        Num_Ligne        : Integer;
    begin
        Open(Entree_Paquets, In_File, To_String(Fichier_Paquets));
        Create(Sortie_Resultats, Out_File, To_String(Fichier_Resultats));

        while not End_Of_File(Entree_Paquets) and Continuer loop
            Num_Ligne := Integer(Line(Entree_Paquets)); 
            
            
            Ligne_P := To_Unbounded_String(Get_Line(Entree_Paquets)); 
            
            Traiter_Ligne(Ligne_P, Num_Ligne, Sortie_Resultats, Continuer, La_Table);
        end loop;

        Close(Entree_Paquets);
        Close(Sortie_Resultats);
    end Traiter_Fichiers_Paquets;

end Routeur;