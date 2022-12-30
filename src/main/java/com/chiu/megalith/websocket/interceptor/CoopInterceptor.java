package com.chiu.megalith.websocket.interceptor;

import com.chiu.megalith.authentication.role.DefaultRoleHolder;
import com.chiu.megalith.authentication.role.HighestRoleHolder;
import com.chiu.megalith.common.exception.AuthenticationExceptionImpl;
import com.chiu.megalith.common.jwt.JwtUtils;
import com.chiu.megalith.websocket.principal.StompPrincipal;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.apache.http.HttpHeaders;
import org.springframework.lang.NonNull;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.simp.stomp.StompCommand;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.messaging.support.ChannelInterceptor;
import org.springframework.messaging.support.MessageHeaderAccessor;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-06-17 9:46 PM
 */
@Component
@RequiredArgsConstructor
public class CoopInterceptor implements ChannelInterceptor {

    private final JwtUtils jwtUtils;

    private final DefaultRoleHolder defaultRoleHolder;

    private final HighestRoleHolder highestRoleHolder;

    private final List<String> roles = new ArrayList<>();


    @PostConstruct
    private void init() {
        roles.addAll(Arrays.stream(defaultRoleHolder.getRole()).toList());
        roles.add(highestRoleHolder.getRole());
    }

    @Override
    public Message<?> preSend(@NonNull Message<?> message,@NonNull MessageChannel channel) {
        StompHeaderAccessor accessor = MessageHeaderAccessor.getAccessor(message, StompHeaderAccessor.class);

        Optional.ofNullable(accessor).ifPresent(o -> {
            if (StompCommand.CONNECT.equals(accessor.getCommand())) {
                String token = accessor.getFirstNativeHeader(HttpHeaders.AUTHORIZATION);
                //验证token是否有效
                Claims claim = jwtUtils.getClaimByToken(token).
                        orElseThrow(() -> new JwtException("token invalid"));

                if (jwtUtils.isTokenExpired(claim.getExpiration())) {
                    throw new JwtException("token expired");
                }

                String userId = claim.getSubject();
                String role = (String) claim.get("role");

                if (!roles.contains(role)) {
                    throw new AuthenticationExceptionImpl("non permit");
                }
                accessor.setUser(new StompPrincipal(userId));

            }
        });

        return message;
    }
}
