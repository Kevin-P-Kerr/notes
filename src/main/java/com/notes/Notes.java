package com.notes;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.math3.analysis.polynomials.PolynomialFunctionLagrangeForm;

import com.google.common.collect.Lists;
import com.google.common.primitives.Doubles;

public class Notes {

	public static long getFactor(long l) {
		long  i =2;
		while (l%i != 0) {
			i++;
		}
		if (i==l) {
			return 1;
		}
		return i;
	}
	
	public static class Tuple<L,R> {
		public final L left;
		public final R right;
		
		public Tuple(L l,R r) {
			this.left = l;
			this.right = r;
		}
		
	}
	
	public static Long fib (long l) {
		long i = 0;
		long ii = 1;
		while (l > 0) {
			long n = i;
			i = ii;
			ii = n+i;
			l--;
		}
		return i;
		
	}
	
	public static long fact(long i) {
		long base = i;
		i--;
		while (i>0) {
			base*=i--;
		}
		return base;
	}
	public static void main(String[] args) {
		List<Tuple<Long,Double>> l = Lists.newArrayList();
		List<Long> xElems = Lists.newArrayList(1L,5L,11L);
		for (Long xelm:xElems) {
			double f = fact(xelm) * 1.0;
			l.add(new Tuple<Long, Double>(xelm,f));
		}
		
		List<Double> x = Lists.newArrayList();
		List<Double> y = Lists.newArrayList();
		for (Tuple<Long, Double> t:l) {
			Long xa = t.left;
			Double ya = t.right;
			x.add(xa.doubleValue());
			y.add(ya.doubleValue());
		}
		PolynomialFunctionLagrangeForm poly = new PolynomialFunctionLagrangeForm(Doubles.toArray(x), Doubles.toArray(y));
		String ps = poly.toString();
		String polystr = "";
		for (int z =0,ii=10;z<=ii;z++) {
			double v = poly.value(z*1.0);
			double fact = fact(z)*1.0;
			System.out.println(String.format("%d: %f : %f",z, v,fact));			
		}
		List<Double> co = Doubles.asList(poly.getCoefficients());
		
		int pow = co.size()-1;
		for (Double c: co) {
			polystr += String.format("%fx^%d ", c,pow);
			pow--;
		}
		System.out.println(polystr);
		

		
	}

}
