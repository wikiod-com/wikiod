---
title: "Teste de unidade"
slug: "teste-de-unidade"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Adicionando projeto de teste de unidade MSTest a uma solução existente
* Clique com o botão direito na solução, Adicionar novo projeto
* Na seção Teste, selecione um projeto de teste de unidade
* Escolha um nome para o assembly - se você estiver testando o projeto `Foo`, o nome pode ser `Foo.Tests`
* Adicione uma referência ao projeto testado nas referências do projeto de teste de unidade


## Criando um método de teste de amostra
MSTest (o framework de teste padrão) requer que você tenha suas classes de teste decoradas por um atributo `[TestClass]`, e os métodos de teste com um atributo `[TestMethod]`, e que sejam públicos.

    [TestClass]
    public class FizzBuzzFixture
    {
        [TestMethod]
        public void Test1()
        {
            //arrange
            var solver = new FizzBuzzSolver();
            //act
            var result = solver.FizzBuzz(1);
            //assert
            Assert.AreEqual("1",result);
        }
    }

