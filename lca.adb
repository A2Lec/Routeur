with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body LCA is

    procedure Liberer is
        new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);


	procedure Initialiser(Sda: out T_LCA) is
	begin
    
    		Sda := null;
	end Initialiser;


	function Est_Vide (Sda : T_LCA) return Boolean is
    	begin
        return Sda = null;
	end;


	procedure Detruire (Sda : in out T_LCA) is
        Temp : T_LCA;
        begin
            while Sda /= null loop
                Temp := Sda.Suivant; 
                Liberer(Sda);           
                Sda := Temp;         
            end loop;
       
        end Detruire;


	function Taille (Sda : in T_LCA) return Integer is
            Compteur : Integer;
            Courant  : T_LCA;
    	begin
        
            Compteur := 0;
            Courant  := Sda;

            while Courant /= null loop
                Compteur := Compteur + 1;
                Courant  := Courant.Suivant;
            end loop;

            return Compteur;
        end Taille;


	function Cle_Presente (Sda : in T_LCA ; Cle : in T_Cle) return Boolean is
        Courant : T_LCA;
        Trouve : Boolean;
        begin
        
            Courant := Sda;
            Trouve  := False;
        
            while Courant /= null and then not Trouve loop
                if Courant.Cle = Cle then
                    Trouve := True;
                end if;
                Courant := Courant.Suivant;
            end loop;

            return Trouve;
        end Cle_Presente;


    function La_Valeur (Sda : in T_LCA ; Cle : in T_Cle) return T_Valeur is
        Courant : T_LCA;
        Valeur_Premiere : T_Valeur;
        begin
            Courant := Sda;

            while Courant /= null loop
                if Courant.Cle = Cle then

                    return Courant.Valeur;
                end if;
                Courant := Courant.Suivant;
            end loop;

            if Sda /= null then
                Valeur_Premiere := Sda.Valeur;
                return Valeur_Premiere;
            end if;

            return Sda.Valeur;
        end La_Valeur;


	procedure Enregistrer (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is
	begin
		Enregistrer_Recursif (Sda, Cle, Valeur);
	end Enregistrer;


	procedure Enregistrer_Iteratif (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is
        Courant : T_LCA;
        begin
            Courant := Sda;

            while Courant /= null loop
                if Courant.Cle = Cle then
                
                    Courant.Valeur := Valeur;
                    return;
                end if;
                Courant := Courant.Suivant;
            end loop;
        
            Sda := new T_Cellule'(Cle => Cle, Valeur => Valeur, Suivant => Sda);
        end Enregistrer_Iteratif;


	procedure Enregistrer_Recursif (Sda : in out T_LCA ; Cle : in T_Cle ; Valeur : in T_Valeur) is
        begin
            if Sda = null then            
                Sda := new T_Cellule'(Cle => Cle, Valeur => Valeur, Suivant => null);

            elsif Sda.Cle = Cle then            
                Sda.Valeur := Valeur;

            else            
                Enregistrer_Recursif(Sda.Suivant, Cle, Valeur);

            end if;
        end Enregistrer_Recursif;


	procedure Supprimer (Sda : in out T_LCA ; Cle : in T_Cle) is
	begin
		Supprimer_Recursif(Sda, Cle);
	end Supprimer;


	procedure Supprimer_Iteratif (Sda : in out T_LCA ; Cle : in T_Cle) is
            Courant : T_LCA;
            Precedent : T_LCA;
        begin
            Courant   := Sda;
            Precedent := null;

            while Courant /= null and then Courant.Cle /= Cle loop
                Precedent := Courant;
                Courant   := Courant.Suivant;
            end loop;

            if Courant = null then
                return;
        
            elsif Precedent = null then
                Sda := Courant.Suivant;
        
            else
                Precedent.Suivant := Courant.Suivant;
            end if;

                Liberer(Courant);
        
        end Supprimer_Iteratif;


	procedure Supprimer_Recursif (Sda : in out T_LCA ; Cle : in T_Cle) is
            Temp_Cellule_A_Liberer : T_LCA;
        begin
            if Sda = null then
                return;
        
            elsif Sda.Cle = Cle then
                Temp_Cellule_A_Liberer := Sda;
                Sda := Sda.Suivant;
                Liberer(Temp_Cellule_A_Liberer);
        
            else
                Supprimer_Recursif(Sda.Suivant, Cle);
            end if;
        end Supprimer_Recursif;


	procedure Faire_Pour_Chaque (Sda : in T_LCA) is
        Courant : T_LCA;
    begin
        Courant := Sda;
        while Courant /= null loop
            begin
                Traiter(Courant.Cle, Courant.Valeur);
            exception
                when others =>
                    null; 
            end;
            Courant := Courant.Suivant;
        end loop;
    end Faire_Pour_Chaque;

     -- -->["un" : 1]-->["deux" : 2]-->["trois" : 3]-->["quatre" : 4]--E
    procedure Afficher_Diagnostic (Sda : in T_LCA) is
            Courant : T_LCA;
        begin
            Courant := Sda;
            while Courant /= null loop
                Put("-->[");
                Afficher_Cle(Courant.Cle);
                Put(" : ");
                Afficher_Donnee(Courant.Valeur);
                Put("]");
                Courant := Courant.Suivant;
            end loop;
                Put("--E");
            end Afficher_Diagnostic;


end LCA;
