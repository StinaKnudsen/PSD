// File Intro/SimpleExpr.java
// Java representation of expressions as in lecture 1
// sestoft@itu.dk * 2010-08-29

import java.util.Map;
import java.util.HashMap;

abstract class Expr { 
  abstract public int eval(Map<String,Integer> env);
  abstract public String fmt();
  abstract public String fmt2(Map<String,Integer> env);
  abstract public String toString();
  abstract public Expr simplify();
}

class CstI extends Expr { 
  protected final int i;

  public CstI(int i) { 
    this.i = i; 
  }

  public int eval(Map<String,Integer> env) {
    return i;
  } 

  public Expr simplify(){
    return this;
  }

  public String fmt() {
    return ""+i;
  }

  public String fmt2(Map<String,Integer> env) {
    return ""+i;
  }

  public String toString(){
    return fmt();
  }
}

class Var extends Expr { 
  protected final String name;

  public Var(String name) { 
    this.name = name; 
  }

  public int eval(Map<String,Integer> env) {
    return env.get(name);
  } 

  public Expr simplify(){
    return this;
  }

  public String fmt() {
    return name;
  } 

  public String fmt2(Map<String,Integer> env) {
    return ""+env.get(name);
  } 

  public String toString(){
    return fmt();
  }

}

class Prim extends Expr { 
  protected final String oper;
  protected final Expr e1, e2;

  public Prim(String oper, Expr e1, Expr e2) { 
    this.oper = oper; this.e1 = e1; this.e2 = e2;
  }

  public int eval(Map<String,Integer> env) {
    if (oper.equals("+"))
      return e1.eval(env) + e2.eval(env);
    else if (oper.equals("*"))
      return e1.eval(env) * e2.eval(env);
    else if (oper.equals("-"))
      return e1.eval(env) - e2.eval(env);
    else
      throw new RuntimeException("unknown primitive");
  } 

  public Expr simplify() {
    Expr e12 = e1.simplify();
    Expr e22 = e2.simplify();
    switch (oper) {
      case "+":
        if (e12 instanceof CstI && e12.fmt().equals("0")) {
          return e22;
        } else if (e22 instanceof CstI && e22.fmt().equals("0")) {
          return e12;
        } else {
          return this;
        }
      case "-":
        if (e22 instanceof CstI && e22.fmt().equals("0")) {
          return e12;
        } else if (e12.fmt().equals(e22.fmt())) {
          return new CstI(0);
        }else {
          return this;
        }
      case "*":
         if (e22 instanceof CstI && e22.fmt().equals("1")) {
          return e12;
        } else if (e12 instanceof CstI && e12.fmt().equals("1")) {
          return e22;
        } else if (e22 instanceof CstI && e22.fmt().equals("0") || e12 instanceof CstI && e12.fmt().equals("0")) {
          return new CstI(0);
        }else {
          return this;
        }
      default:
        return this;
    }
  }

  public String fmt() {
    return "(" + e1.fmt() + oper + e2.fmt() + ")";
  } 

  public String fmt2(Map<String,Integer> env) {
    return "(" + e1.fmt2(env) + oper + e2.fmt2(env) + ")";
  } 

  public String toString(){
    return fmt();
  }

}

public class SimpleExpr {
  public static void main(String[] args) {
    Expr e1 = new CstI(17);
    Expr e2 = new Prim("+", new CstI(3), new Var("a"));
    Expr e3 = new Prim("+", new Prim("*", new Var("b"), new CstI(9)), 
		            new Var("a"));
    Expr e4 = new Prim("*", new Prim("+", new CstI(10), new CstI(5)), new CstI(2));
    Expr e5 = new Prim("-", new Prim("+", new CstI(10), new CstI(5)), new CstI(2));
    Expr e6 = new Prim("+", new Prim("*", new Prim("-", new CstI(10), new CstI(3)), new CstI(5)), new CstI(2));
    Expr e7 = new Prim("+", new CstI(0), new Var("a")); // Example of simplifying
    Map<String,Integer> env0 = new HashMap<String,Integer>();
    env0.put("a", 3);
    env0.put("c", 78);
    env0.put("baf", 666);
    env0.put("b", 111);

    System.out.println("Env: " + env0.toString());

    System.out.println("E1:");
    System.out.println(e1.fmt() + " = " + e1.fmt2(env0) + " = " + e1.eval(env0));
    System.out.println("E2:");
    System.out.println(e2.fmt() + " = " + e2.fmt2(env0) + " = " + e2.eval(env0));
    System.out.println("E3:");
    System.out.println(e3.fmt() + " = " + e3.fmt2(env0) + " = " + e3.eval(env0));
    System.out.println("E4:");
    System.out.println(e4.fmt() + " = " + e4.fmt2(env0) + " = " + e4.eval(env0));
    System.out.println("E5:");
    System.out.println(e5.fmt() + " = " + e5.fmt2(env0) + " = " + e5.eval(env0));
    System.out.println("E6:");
    System.out.println(e6.fmt() + " = " + e6.fmt2(env0) + " = " + e6.eval(env0));
    System.out.println("E7:");
    System.out.println(e7.fmt() + " = " + e7.simplify().fmt()); // Simplify example
  }
}
