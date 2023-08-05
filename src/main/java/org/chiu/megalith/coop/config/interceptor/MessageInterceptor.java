package org.chiu.megalith.coop.config.interceptor;

import org.chiu.megalith.infra.jwt.JwtUtils;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import lombok.RequiredArgsConstructor;

import java.util.Objects;
import org.apache.http.HttpHeaders;
import org.springframework.lang.NonNull;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.simp.stomp.StompCommand;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.messaging.support.ChannelInterceptor;
import org.springframework.messaging.support.MessageHeaderAccessor;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationToken;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * @author mingchiuli
 * @create 2022-06-17 9:46 PM
 */
@Component
@RequiredArgsConstructor
public class MessageInterceptor implements ChannelInterceptor {

    private final JwtUtils jwtUtils;

    @Override
    public Message<?> preSend(@NonNull Message<?> message,
            @NonNull MessageChannel channel) {
        StompHeaderAccessor accessor = MessageHeaderAccessor.getAccessor(message, StompHeaderAccessor.class);
        Assert.isTrue(Objects.nonNull(accessor), "accessor is null");
        if (StompCommand.CONNECT.equals(accessor.getCommand())) {
            String token = accessor.getFirstNativeHeader(HttpHeaders.AUTHORIZATION);
            if (StringUtils.hasLength(token)) {
                String jwt;
                try {
                    jwt = token.substring("Bearer ".length());
                } catch (IndexOutOfBoundsException e) {
                    throw new JwtException("token invalid");
                }
                Claims claim = jwtUtils.getClaimByToken(jwt)
                        .orElseThrow(() -> new JwtException("token invalid"));

                if (jwtUtils.isTokenExpired(claim.getExpiration())) {
                    throw new JwtException("token expired");
                }

                String userId = claim.getSubject();
                String role = (String) claim.get("role");

                accessor.setUser(new PreAuthenticatedAuthenticationToken(userId, null,
                        AuthorityUtils.createAuthorityList(role)));
            }

        }

        return message;
    }
}
