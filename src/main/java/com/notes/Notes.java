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
	public static void main(String[] args) {
		List<Tuple<Long,Long>> l = Lists.newArrayList();
		List<Long> xElems = Lists.newArrayList(9L,11L,21L,33L,47L,55L,67L,71L,85L,91L,105L);
		for (Long xelm:xElems) {
			long f = getFactor(xelm);
			l.add(new Tuple<Long, Long>(xelm,f));
		}
		
		List<Double> x = Lists.newArrayList();
		List<Double> y = Lists.newArrayList();
		for (Tuple<Long, Long> t:l) {
			Long xa = t.left;
			Long ya = t.right;
			x.add(xa.doubleValue());
			y.add(ya.doubleValue());
		}
		PolynomialFunctionLagrangeForm poly = new PolynomialFunctionLagrangeForm(Doubles.toArray(x), Doubles.toArray(y));
		String ps = poly.toString();
		String polystr = "";
		for (int z =3,ii=105;z<=ii;z+=2) {
			double v = poly.value(z*1.0);
			long fv = Math.round(v);
			long fact = getFactor(z);
			System.out.println(String.format("%d: %d : %d",z, fv,fact));			
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
