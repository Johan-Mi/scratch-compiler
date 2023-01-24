use crate::uid::Uid;
use core::fmt;
use std::collections::HashMap;

pub type Broadcasts = HashMap<String, (Uid, Vec<Uid>)>;

pub fn write_broadcast_handlers(
    broadcasts: &Broadcasts,
    f: &mut fmt::Formatter,
) -> fmt::Result {
    if broadcasts.is_empty() {
        return Ok(());
    }

    for (broadcast, receivers) in broadcasts.values() {
        writeln!(f, "{broadcast}:")?;
        let (last, rest) = receivers.split_last().unwrap();
        if !rest.is_empty() {
            f.write_str("    sub rsp, 8\n")?;
            for receiver in rest {
                writeln!(f, "    call {receiver}")?;
            }
            f.write_str("    add rsp, 8\n")?;
        }
        writeln!(f, "    jmp {last}")?;
    }

    writeln!(
        f,
        "\nsend_broadcast:
    mov eax, 60
    mov edi, 80
    syscall"
    )?;

    Ok(())
}
