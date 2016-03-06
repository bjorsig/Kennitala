using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace csKennitala
{
    /// <summary>
    /// Klasinn skilgreinir aðgerðir sem hægt er að gera á íslenska kennitölu
    /// </summary>
    public class Kennitala
    {
        private string kt = string.Empty;
        public string Kt
        {
            get { return kt; }
            set
            {
                if (TolustafirEingongu(value))
                {
                    kt = value;
                }
                else
                {
                    throw new ArgumentException("Kennitölustrengur má bara innihalda tölustafi");
                }
            }
        }
        
        /// <summary>
        /// Tegund kennitölu; einstaklingur eða fyrirtæki
        /// </summary>
        public tegundKennitolu TegundKennitolu 
        {
            get { return FaTegund(); }
        }

        public Kennitala(string kt)
        {
            if (TolustafirEingongu(kt))
            {
                Kt = kt;
            }
            else
            {
                throw new ArgumentException("Kennitölustrengur getur ekki verið löglegur");
            }

        }

        /// <summary>
        /// Segir til um hvort kenitala er lögleg, þ.e. "well formed". En ekki hvort hún tilheyri einhverjum.
        /// </summary>
        /// <returns>Kennitala stenst vartölupróf</returns>
        public bool ErLogleg()
        {
            if ((! string.IsNullOrWhiteSpace(Kt)) && Kt.Count() == 10)
            {
                string stak0 = Kt.Substring(0, 1);
                string stak9 = Kt.Substring(9, 1);
                try 
                {
                    if (new int[] {0,1,2,3,4,5,6,7}.Contains(Int32.Parse(stak0))  
                        && new int[] {0,8,9}.Contains(Int32.Parse(stak9)) )
                    { //við erum með 10 staka streng sem byrjar og endar á réttri tölu
                        if (Int32.Parse(Kt.Substring(8, 1)) == FaVartolu())
                        { return true; }
                        else { return false; }
                    }
                } catch (Exception e)
                {
                    return false;
                }
            }
            return false;
        }

        /// <summary>
        /// Reiknar fæðingardag eða stofndag út frá kennitölu.
        /// Kastar villu ef kennitala er ólögleg og ekki hægt að reikna dag.
        /// </summary>
        /// <returns>Fæðingar- eða stofndegi sem skilgreind kennitala gefur til kynna.</returns>
        public DateTime FaFaedingardag()
        {
            if (ErLogleg())
            {
                try
                {
                    int ar = 0;
                    switch (Kt.Substring(9, 1))
                    {
                        case "0":
                            ar = 2000;
                            break;
                        case "8":
                            ar = 1800;
                            break;
                        case "9":
                            ar = 1900;
                            break;
                    }
                    ar += Int32.Parse(Kt.Substring(4, 2));
                    if (Int32.Parse(Kt.Substring(0, 1)) > 3)
                    { 
                        int day = (Int32.Parse(Kt.Substring(0, 1)) - 4) * 10 + Int32.Parse(Kt.Substring(1,1));
                        return new DateTime(ar, Int32.Parse(Kt.Substring(2, 2)), day); 
                    }
                    else
                    { return new DateTime(ar, Int32.Parse(Kt.Substring(2, 2)), Int32.Parse(Kt.Substring(0, 2))); }
                } catch (Exception e)
                {
                    throw new Exception("Ekki hægt að sækja dag úr kennitölu", e);
                }
            }
            throw new Exception("Ekki hægt að sækja dag úr ólöglegri kennitölu");
        }

        /// <summary>
        /// Reiknar vartölu fyrir kennitölu, þ.e. 9. stafinn í kennitölunni.    
        /// Notar aðeins 8 stafi og því er mögulegt að skilgreina 8 stafi og reikna þann níunda.
        /// </summary>
        /// <returns>Vartölu (níunda staf) fyrir skilgreinda kennitölu</returns>
        public int FaVartolu()
        {
            try
            {
                int sum = 0;
                for (int i = 0; i < 8; i++)
                {
                    sum += int.Parse(Kt.Substring(i, 1)) * margfeldi[i];
                }
                int vartala = 11 - (sum % 11);
                if (vartala == 11)
                {
                    return 0;
                } else if (vartala == 10)
                {
                    throw new Exception("Ekki hægt að reikna vartölu fyrir þennan kennitöluhluta");
                } else
                { return vartala; }
            } catch (Exception e)
            {
                throw new Exception("Ekki hægt að reikna vartölu fyrir þennan kennitöluhluta", e);
            }
        }

        private tegundKennitolu FaTegund()
        {
            if (! string.IsNullOrWhiteSpace(Kt))
            {
                int elm0;
                string str0 = Kt.Substring(0, 1);
                if (Int32.TryParse(str0, out elm0))
                {
                    if (new List<string>() {"0", "1", "2", "3"}.Contains(str0) )
                    {
                        return tegundKennitolu.einstaklingur;
                    } else if (new List<string>(){"4", "5", "6", "7"}.Contains(str0))
                    {
                        return tegundKennitolu.fyrirtaeki;
                    } else
                    {
                        throw new Exception("Kennitölustrengur getur ekki verið löglegur");
                    }
                }
            }
            throw new Exception("Kennitölustrengur getur ekki verið löglegur");
        }
        bool TolustafirEingongu(string str)
        {
            foreach (char c in str)
            {
                if (c < '0' || c > '9')
                    return false;
            }

            return true;
        }

        private int[] margfeldi = new int[8] {3,2,7,6,5,4,3,2};
    }

    public enum tegundKennitolu
    { einstaklingur, fyrirtaeki}
}
