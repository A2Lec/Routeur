with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Adresse_IP;            use Adresse_IP;

package body Trie is

    procedure Free is new Ada.Unchecked_Deallocation(
        Object => T_Cellule_Trie,
        Name => T_Trie);

    -- recuperer un bit de l'adresse
    function Get_Bit(Adresse : T_IP; Position : Integer) return T_Bit is
        num_octet : Integer;
        bit_octet : Integer;
        val : Integer;
        ind : Integer;
        m : Integer;
        -- temp : Integer; -- pour debug
    begin
        num_octet := ((Position - 1) / 8) + 1;
        bit_octet := (Position - 1) mod 8;
        
        Kieme_Adresse(num_octet, Adresse, ind, val);
        
        m := 2 ** (7 - bit_octet);
        
        if (val / m) mod 2 = 1 then
            return 1;
        else
            return 0;
        end if;
    end Get_Bit;

    
    procedure Initialiser(Trie : out T_Trie) is
    begin
        Trie := null;
    end Initialiser;

    
    function EstVide(Trie : in T_Trie) return Boolean is
    begin
        return Trie = null;
    end EstVide;

    function EstFeuille(Trie : in T_Trie) return Boolean is
    begin
        if EstVide(Trie) then
            return False;
        else
            if Trie /= null then
                return EstVide(Trie.Enfants(0)) and EstVide(Trie.Enfants(1));
            else
                return False;
            end if;
        end if;
    end EstFeuille;

    function Taille(Trie : in T_Trie) return Integer is
        c : Integer := 0;
    begin
        if EstVide(Trie) then
            return 0;
        end if;
        
        if Trie.Est_Route then
            c := 1;
        end if;
        
        c := c + Taille(Trie.Enfants(0));
        c := c + Taille(Trie.Enfants(1));
        
        return c;
    end Taille;

    
    procedure Ajouter(Trie : in out T_Trie; 
                      Adresse : in T_IP;
                      Longueur_Prefixe : in T_Longueur_Prefixe;
                      Interface_Sortie : in Unbounded_String) is
        
        procedure Ajouter_Rec(Noeud : in out T_Trie; prof : Integer) is
            b : T_Bit;
            -- ancien_noeud : T_Trie;
        begin
            -- if prof = 1 then
            --     Put_Line("Ajout racine");
            -- end if;
            if EstVide(Noeud) then
                Noeud := new T_Cellule_Trie;
                Noeud.Enfants(0) := null;
                Noeud.Enfants(1) := null;
                Noeud.Est_Route := False;
                Noeud.Interface_Sortie := Null_Unbounded_String;
            end if;
            
            if prof > Longueur_Prefixe then
                Noeud.Est_Route := True;
                Noeud.Interface_Sortie := Interface_Sortie;
            else
                b := Get_Bit(Adresse, prof);
                Ajouter_Rec(Noeud.Enfants(b), prof + 1);
            end if;
        end Ajouter_Rec;
        
    begin
        Ajouter_Rec(Trie, 1);
    end Ajouter;

    -- recherche avec plus long prefixe
    function Rechercher(Trie : in T_Trie; 
                        Adresse : in T_IP;
                        Interface_Sortie : out Unbounded_String) return Boolean is
        Trouve : Boolean := False;
        nb_recherches : Integer := 0;
        
        procedure Rech_Rec(Noeud : T_Trie; p : Integer) is
            b : T_Bit;
        begin
            if EstVide(Noeud) or Noeud = null then
                return;
            end if;
            
            if Noeud.Est_Route and Noeud /= null then
                Interface_Sortie := Noeud.Interface_Sortie;
                Trouve := True;
            end if;
            
            if p <= 32 then
                b := Get_Bit(Adresse, p);
                Rech_Rec(Noeud.Enfants(b), p + 1);
            end if;
        end Rech_Rec;
        
    begin
        Interface_Sortie := Null_Unbounded_String;
        Rech_Rec(Trie, 1);
        return Trouve;
    end Rechercher;

    
    procedure Supprimer(Trie : in out T_Trie; 
                        Adresse : in T_IP;
                        Longueur_Prefixe : in T_Longueur_Prefixe) is
        
        procedure Supp_Rec(Noeud : in out T_Trie; p : Integer) is
            b : T_Bit;
        begin
            if EstVide(Noeud) then
                return;
            end if;
            
            if p > Longueur_Prefixe then
                Noeud.Est_Route := False;
                Noeud.Interface_Sortie := Null_Unbounded_String;
            else
                b := Get_Bit(Adresse, p);
                Supp_Rec(Noeud.Enfants(b), p + 1);
            end if;
        end Supp_Rec;
        
    begin
        Supp_Rec(Trie, 1);
    end Supprimer;

    
    procedure Vider(Trie : in out T_Trie) is
        compteur : Integer := 0;
    begin
        if not EstVide(Trie) then
            Vider(Trie.Enfants(0));
            Vider(Trie.Enfants(1));
            -- compteur := compteur + 1;
            Free(Trie);
        end if;
    end Vider;

    
    procedure Afficher(Trie : in T_Trie) is
        
        procedure Aff_Rec(Noeud : T_Trie; 
                          Pref : String;
                          Prof : Integer) is
        begin
            if EstVide(Noeud) then
                return;
            end if;
            
            if Noeud.Est_Route then
                Put("  ");
                Put(Pref);
                Put(" (/" & Integer'Image(Prof)(2..Integer'Image(Prof)'Last) & ")");
                Put(" -> ");
                Put_Line(To_String(Noeud.Interface_Sortie));
            end if;
            
            Aff_Rec(Noeud.Enfants(0), Pref & "0", Prof + 1);
            Aff_Rec(Noeud.Enfants(1), Pref & "1", Prof + 1);
        end Aff_Rec;
        
    begin
        Put_Line(" Contenu du Cache (Trie) ");
        if EstVide(Trie) then
            Put_Line("  (vide)");
        else
            Aff_Rec(Trie, "", 0);
        end if;
        Put_Line("===============================");
    end Afficher;

end Trie;
