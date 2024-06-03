package org.chiu.megalith.security.token;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.Algorithm;
import com.nimbusds.jose.JOSEObjectType;
import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jose.JWSObject;
import com.nimbusds.jose.JWSSigner;
import com.nimbusds.jose.JWSVerifier;
import com.nimbusds.jose.Payload;
import com.nimbusds.jose.crypto.MACSigner;
import com.nimbusds.jose.crypto.MACVerifier;
import com.nimbusds.jose.jwk.source.JWKSetParseException;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;

import jakarta.annotation.PostConstruct;
import lombok.Data;
import lombok.SneakyThrows;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

import java.util.Date;
import java.util.List;

/**
 * jwt工具类
 */
@Data
@Component
@ConfigurationProperties(prefix = "blog.jwt")
public class JwtUtils implements TokenUtils<Claims> {

    private String secret;

    private Algorithm algorithm;

    private JWSVerifier verifier;

    private JWSSigner signer;

    private final ObjectMapper objectMapper;

    @PostConstruct
    @SneakyThrows
    public void init() {
        verifier = new MACVerifier(secret);
        signer = new MACSigner(secret);
    }

    @SneakyThrows
    public String generateToken(String userId, List<String> roles, long expire) {
        JWSHeader jwsHeader = new JWSHeader.Builder(JWSAlgorithm.HS512)
                .type(JOSEObjectType.JWT)
                .build();

        var nowDate = new Date();
        // 过期时间
        var expireDate = new Date(nowDate.getTime() + expire * 1000);
        JWTClaimsSet jwtClaimsSet = new JWTClaimsSet.Builder()
                .issuer("megalith")
                .subject(userId)
                .claim("roles", roles)
                .issueTime(nowDate)
                .expirationTime(expireDate)
                .build();

        Payload payload = new Payload(jwtClaimsSet.toJSONObject());
        JWSObject jwsObject = new JWSObject(jwsHeader, payload);
        jwsObject.sign(signer);
        return jwsObject.serialize();
    }

    @SuppressWarnings("unchecked")
    @SneakyThrows
    public Claims getVerifierByToken(String token) {
        SignedJWT signedJWT = SignedJWT.parse(token);
        if (!signedJWT.verify(verifier)) {
            throw new JWKSetParseException(AUTH_EXCEPTION.getMsg(), null);
        }

        JWTClaimsSet jwtClaimsSet = signedJWT.getJWTClaimsSet();

        Date expirationTime = jwtClaimsSet.getExpirationTime();
        if (new Date().after(expirationTime)) {
            throw new JWKSetParseException(TOKEN_INVALID.getMsg(), null);
        }

        var claim = new Claims();

        Object roles = jwtClaimsSet.getClaim("roles");
        claim.setUserId(jwtClaimsSet.getSubject());
        claim.setRoles((List<String>) roles);
        return claim;
    }
}
