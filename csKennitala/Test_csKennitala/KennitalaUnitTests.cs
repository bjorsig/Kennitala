using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using csKennitala;

namespace Test_csKennitala
{
    [TestClass]
    public class KennitalaUnitTests
    {
        [TestMethod]
        public void TestFaVartolu()
        {
            Kennitala kt = new Kennitala("5108891269");
            Assert.AreEqual(6, kt.FaVartolu());

            kt = new Kennitala("17034370"); //1703437009
            Assert.AreEqual(0, kt.FaVartolu());

            try
            {
                kt = new Kennitala("010101"); //of stutt
                var vartala = kt.FaVartolu();
                Assert.Fail("FaVartolu ætti að kasta villu");
            } catch (Exception e)
            {
                Assert.AreEqual("Ekki hægt að reikna vartölu fyrir þennan kennitöluhluta", e.Message);
            }
        }

        [TestMethod]
        public void TestTegund()
        {
            Kennitala kt = new Kennitala("1709715079");
            Assert.AreEqual(kt.TegundKennitolu, tegundKennitolu.einstaklingur);
            
            kt = new Kennitala("4403044350");
            Assert.AreEqual(kt.TegundKennitolu, tegundKennitolu.fyrirtaeki);

            try
            { 
                kt = new Kennitala("abcdefghij");
                var teg = kt.TegundKennitolu;
                Assert.Fail("Hefði átt að kasta exception. Kennitala getur ekki verið lögleg");
            } catch (Exception e)
            {
                Assert.AreEqual("Kennitölustrengur getur ekki verið löglegur", e.Message);
            }
        }

        [TestMethod]
        public void TestErLogleg()
        {
            //strengir sem non-numeric chars
            Kennitala kt = new Kennitala("abcdefghij"); //vitlaust type
            Assert.IsFalse(kt.ErLogleg());
            kt.Kt = "11111111.9";
            Assert.IsFalse(kt.ErLogleg());

            // röng lengd af kennitölu
            kt.Kt = "222222229";
            Assert.IsFalse(kt.ErLogleg());
            kt.Kt = "33333333339";
            Assert.IsFalse(kt.ErLogleg());

            //röng vartala
            kt.Kt = "1511825439";
            Assert.IsFalse(kt.ErLogleg());
            kt.Kt = "1211912119";
            Assert.IsFalse(kt.ErLogleg());

            //réttar kennitölur
            kt.Kt = "6909952209";
            Assert.IsTrue(kt.ErLogleg());
            kt.Kt = "4403044350";
            Assert.IsTrue(kt.ErLogleg());
            kt.Kt = "0404754788";
            Assert.IsTrue(kt.ErLogleg());
        }

        [TestMethod]
        public void TestFaFaedingardag()
        {
            try
            {
                //einstaklingar
                Kennitala kt = new Kennitala("1204763359");
                DateTime dagur = new DateTime(1976, 4, 12);
                Assert.AreEqual(dagur, kt.FaFaedingardag());
                kt.Kt = "0205013080";
                dagur = new DateTime(2001, 5, 2);
                Assert.AreEqual(dagur, kt.FaFaedingardag());
                kt.Kt = "1112507588";
                dagur = new DateTime(1850, 12, 11);
                Assert.AreEqual(dagur, kt.FaFaedingardag());

                //rekstraraðilar
                kt.Kt = "4704051470";
                dagur = new DateTime(2005, 4, 7);
                Assert.AreEqual(dagur, kt.FaFaedingardag());
                kt.Kt = "5210992409";
                dagur = new DateTime(1999, 10, 12);
                Assert.AreEqual(dagur, kt.FaFaedingardag());
                kt.Kt = "ólögleg kennitala";
                dagur = kt.FaFaedingardag();
                Assert.Fail("Ætti að hafa kastað villu");
            }
            catch (ArgumentException ex)
            {
                Assert.AreEqual("Kennitölustrengur má bara innihalda tölustafi", ex.Message);
            }
            catch (Exception ex)
            {
                Assert.AreEqual("Ekki hægt að sækja dag úr ólöglegri kennitölu", ex.Message);
            }
            
        }
    }
}
