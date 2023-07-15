package org.chiu.megalith.manage.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.Hibernate;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.GenericGenerator;

import java.io.Serial;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:45 am
 */

@Entity
@Table(name ="m_role_menu")
@Getter
@AllArgsConstructor
@Builder
@Setter
@ToString
@RequiredArgsConstructor
@DynamicUpdate
public class RoleMenuEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 1984295938385285L;

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "native")
    @GenericGenerator(name = "native")
    private Long id;

    @Column(name = "role_id")
    private Long roleId;

    @Column(name = "menu_id")
    private Long menuId;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        RoleMenuEntity that = (RoleMenuEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}
