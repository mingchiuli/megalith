package org.chiu.megalith.security.token;

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

import jakarta.annotation.PostConstruct;
import lombok.Data;
import lombok.SneakyThrows;

import org.chiu.megalith.infra.utils.JsonUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

import java.util.Date;

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

    private final JsonUtils jsonUtils;

    @PostConstruct
    @SneakyThrows
    public void init() {
        verifier = new MACVerifier(secret);
        signer = new MACSigner(secret);
    }

    @SneakyThrows
    public String generateToken(String userId, String role, long expire) {
        JWSHeader jwsHeader = new JWSHeader.Builder(JWSAlgorithm.HS512)
                .type(JOSEObjectType.JWT)
                .build();

        var nowDate = new Date();
        // 过期时间
        var expireDate = new Date(nowDate.getTime() + expire * 1000);
        JWTClaimsSet jwtClaimsSet = new JWTClaimsSet.Builder()
                .issuer("megalith")
                .subject(userId)
                .claim("role", role)
                .issueTime(nowDate)
                .expirationTime(expireDate)
                .build();

        Payload payload = new Payload(jwtClaimsSet.toJSONObject());
        JWSObject jwsObject = new JWSObject(jwsHeader, payload);
        jwsObject.sign(signer);
        return jwsObject.serialize();
    }

    @SneakyThrows
    public Claims getVerifierByToken(String token) {
        var jwsObject = JWSObject.parse(token);
        if (!jwsObject.verify(verifier)) {
            throw new JWKSetParseException(TOKEN_INVALID.getMsg(), null);
        }
        String objStr = jwsObject.getPayload().toString();
        return jsonUtils.readValue(objStr, Claims.class);
    }
}
