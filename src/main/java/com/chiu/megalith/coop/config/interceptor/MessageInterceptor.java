package com.chiu.megalith.coop.config.interceptor;

import com.chiu.megalith.common.jwt.JwtUtils;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import lombok.RequiredArgsConstructor;
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
        if (StompCommand.CONNECT.equals(accessor.getCommand())) {

            String token = accessor.getFirstNativeHeader(HttpHeaders.AUTHORIZATION);
            Claims claim = jwtUtils.getClaimByToken(token).
                    orElseThrow(() -> new JwtException("token invalid"));

            if (jwtUtils.isTokenExpired(claim.getExpiration())) {
                throw new JwtException("token expired");
            }

            String userId = claim.getSubject();
            String role = (String) claim.get("role");

            accessor.setUser(new PreAuthenticatedAuthenticationToken(userId, null, AuthorityUtils.createAuthorityList(role)));
        }

        return message;
    }
}
