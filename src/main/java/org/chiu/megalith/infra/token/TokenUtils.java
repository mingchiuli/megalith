package org.chiu.megalith.infra.token;

public interface TokenUtils<T> {

    String generateToken(String userId, String role, long expire);

    T getVerifierByToken(String token);
}
